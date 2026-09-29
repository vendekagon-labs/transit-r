# A msgpack reader and writer covering what transit needs, in plain R. Maps
# are read with their entries in the order they were written, which the
# transit cache depends on, and 64 bit integers are kept exact.

## Writing

be16 <- function(n) as.raw(c(n %/% 256, n %% 256))
be32 <- function(n) as.raw(c(n %/% 16777216, (n %/% 65536) %% 256, (n %/% 256) %% 256, n %% 256))

mp_header <- function(n, fix, c16, c32) {
  if (n <= 15) as.raw(fix + n)
  else if (n <= 65535) c(as.raw(c16), be16(n))
  else c(as.raw(c32), be32(n))
}

mp_str <- function(s) {
  bytes <- charToRaw(enc2utf8(s))
  n <- length(bytes)
  header <- if (n <= 31) as.raw(0xa0 + n)
    else if (n <= 255) as.raw(c(0xd9, n))
    else if (n <= 65535) c(as.raw(0xda), be16(n))
    else c(as.raw(0xdb), be32(n))
  c(header, bytes)
}

mp_float <- function(x) c(as.raw(0xcb), writeBin(as.double(x), raw(), size = 8, endian = "big"))

# The smallest msgpack encoding of the integer in canonical string s, as
# transit-java writes it.
mp_int <- function(s) {
  if (int32_in_range(s)) {
    n <- as.numeric(s)
    if (n >= 0) {
      if (n <= 127) return(as.raw(n))
      if (n <= 255) return(as.raw(c(0xcc, n)))
      if (n <= 65535) return(c(as.raw(0xcd), be16(n)))
      return(c(as.raw(0xce), be32(n)))
    }
    if (n >= -32) return(as.raw(n + 256))
    if (n >= -128) return(as.raw(c(0xd0, n + 256)))
    if (n >= -32768) return(c(as.raw(0xd1), be16(n + 65536)))
    return(c(as.raw(0xd2), be32(n + 4294967296)))
  }
  if (s == "-2147483648") return(c(as.raw(0xd2), be32(2147483648)))
  if (!startsWith(s, "-") && magnitude_le(s, "4294967295")) return(c(as.raw(0xce), be32(as.numeric(s))))
  c(as.raw(if (startsWith(s, "-")) 0xd3 else 0xcf), int64_to_bytes(s))
}

## Reading

# Reads msgpack from raw bytes, or from a connection, taking from the
# connection exactly the bytes each value needs (so it never waits for data
# beyond the value being read).
mp_reader <- function(bytes = raw(), con = NULL) {
  r <- new.env(parent = emptyenv())
  r$buf <- bytes
  r$pos <- 1L
  r$con <- con
  r
}

mp_eof <- function() stop(structure(class = c("transit_eof", "error", "condition"),
                                    list(message = "Stream ended in the middle of a msgpack value", call = NULL)))

mp_take <- function(r, n) {
  if (n == 0) return(raw())
  end <- r$pos + n - 1L
  if (end > length(r$buf)) {
    need <- end - length(r$buf)
    more <- if (is.null(r$con)) raw() else readBin(r$con, "raw", need)
    if (length(more) < need) mp_eof()
    r$buf <- c(r$buf, more)
  }
  out <- r$buf[r$pos:end]
  r$pos <- end + 1L
  out
}

mp_at_end <- function(r) {
  if (r$pos <= length(r$buf)) return(FALSE)
  if (is.null(r$con)) return(TRUE)
  # Nothing buffered: done with what was read, and waiting for the next value.
  r$buf <- raw()
  r$pos <- 1L
  b <- readBin(r$con, "raw", 1L)
  if (length(b) == 0) return(TRUE)
  r$buf <- b
  FALSE
}

u8 <- function(r) as.integer(mp_take(r, 1L))
u16 <- function(r) readBin(mp_take(r, 2L), "integer", size = 2, signed = FALSE, endian = "big")
u32 <- function(r) { b <- as.numeric(mp_take(r, 4L)); sum(b * c(16777216, 65536, 256, 1)) }

mp_string <- function(r, n) {
  s <- rawToChar(mp_take(r, n))
  Encoding(s) <- "UTF-8"
  s
}

mp_array <- function(r, n) {
  out <- vector("list", n)
  for (i in seq_len(n)) out[i] <- list(mp_unpack(r))
  out
}

mp_map <- function(r, n) {
  keys <- vector("list", n)
  values <- vector("list", n)
  for (i in seq_len(n)) {
    keys[i] <- list(mp_unpack(r))
    values[i] <- list(mp_unpack(r))
  }
  wire_map(keys, values)
}

# R's integer NA is -2^31, so that value becomes a transit_int64.
mp_int32 <- function(r) {
  v <- readBin(mp_take(r, 4L), "integer", size = 4, endian = "big")
  if (is.na(v)) integer_value("-2147483648") else v
}

# Reads one msgpack value.
mp_unpack <- function(r) {
  b <- u8(r)
  if (b <= 0x7f) return(b)
  if (b >= 0xe0) return(b - 256L)
  if (b <= 0x8f) return(mp_map(r, b - 0x80))
  if (b <= 0x9f) return(mp_array(r, b - 0x90))
  if (b <= 0xbf) return(mp_string(r, b - 0xa0))
  switch(as.character(b),
    "192" = NULL,                                                   # 0xc0
    "194" = FALSE,
    "195" = TRUE,
    "196" = mp_take(r, u8(r)),                                      # bin
    "197" = mp_take(r, u16(r)),
    "198" = mp_take(r, u32(r)),
    "202" = readBin(mp_take(r, 4L), "double", size = 4, endian = "big"),  # 0xca
    "203" = readBin(mp_take(r, 8L), "double", size = 8, endian = "big"),
    "204" = u8(r),                                                  # 0xcc
    "205" = u16(r),
    "206" = integer_value(format(u32(r), scientific = FALSE)),
    "207" = integer_value(bytes_to_int64(mp_take(r, 8L), signed = FALSE)),
    "208" = readBin(mp_take(r, 1L), "integer", size = 1, signed = TRUE),  # 0xd0
    "209" = readBin(mp_take(r, 2L), "integer", size = 2, signed = TRUE, endian = "big"),
    "210" = mp_int32(r),
    "211" = integer_value(bytes_to_int64(mp_take(r, 8L))),
    "217" = mp_string(r, u8(r)),                                    # 0xd9
    "218" = mp_string(r, u16(r)),
    "219" = mp_string(r, u32(r)),
    "220" = mp_array(r, u16(r)),                                    # 0xdc
    "221" = mp_array(r, u32(r)),
    "222" = mp_map(r, u16(r)),
    "223" = mp_map(r, u32(r)),
    "193" = stop("invalid msgpack data (0xc1)", call. = FALSE),
    stop(sprintf("msgpack extension types aren't used by transit (type byte 0x%02x)", b), call. = FALSE))
}
