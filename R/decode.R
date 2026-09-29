# Turns parsed JSON or msgpack data (the "wire" form) into R values.
#
# Wire nodes are: NULL, logical(1), integer(1) or transit_int64/bigint,
# double(1), character(1), raw (msgpack bin), unnamed lists (arrays), named
# lists (JSON objects) and wire maps (msgpack maps, whose keys needn't be
# strings).

wire_map <- function(keys, values) structure(list(keys = keys, values = values), class = "transit_wire_map")

tag_marker <- function(tag) structure(list(tag = tag), class = "transit_tag_marker")

## Dates

date_to_ms <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x) * 86400000)
  round(as.numeric(as.POSIXct(x)) * 1000)
}

ms_to_date <- function(ms) .POSIXct(ms / 1000, tz = "UTC")

format_rfc3339 <- function(ms) {
  secs <- floor(ms / 1000)
  paste0(format(.POSIXct(secs, tz = "UTC"), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
         sprintf(".%03dZ", as.integer(ms - secs * 1000)))
}

RFC3339 <- "^([0-9]{4}-[0-9]{2}-[0-9]{2})[Tt ]([0-9]{2}:[0-9]{2}:[0-9]{2})(\\.[0-9]+)?([Zz]|[+-][0-9]{2}:[0-9]{2})?$"

parse_rfc3339 <- function(s) {
  m <- regmatches(s, regexec(RFC3339, s))[[1]]
  if (length(m) == 0) stop("don't know how to parse date/time: ", s, call. = FALSE)
  secs <- as.numeric(as.POSIXct(paste(m[2], m[3]), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  ms <- secs * 1000
  if (nzchar(m[4])) ms <- ms + as.numeric(substr(paste0(substring(m[4], 2), "00"), 1, 3))
  offset <- m[5]
  if (nzchar(offset) && !(offset %in% c("Z", "z"))) {
    minutes <- 60 * as.numeric(substr(offset, 2, 3)) + as.numeric(substr(offset, 5, 6))
    ms <- ms - (if (startsWith(offset, "-")) -1 else 1) * minutes * 60000
  }
  ms_to_date(ms)
}

decode_date <- function(rep) {
  if (is.character(rep) && !inherits(rep, c("transit_int64", "transit_bigint"))) {
    if (grepl("T", rep, fixed = TRUE)) return(parse_rfc3339(rep))
    return(ms_to_date(as.numeric(rep)))
  }
  ms_to_date(as.numeric(unclass(rep)))
}

## UUIDs

# A uuid is a string, or two signed 64 bit ints (most significant first).
decode_uuid <- function(rep) {
  if (is.character(rep) && length(rep) == 1) return(transit_uuid(rep))
  bytes <- c(int64_to_bytes(int_string(rep[[1]])), int64_to_bytes(int_string(rep[[2]])))
  hex <- paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  transit_uuid(paste(substring(hex, c(1, 9, 13, 17, 21), c(8, 12, 16, 20, 32)), collapse = "-"))
}

int_string <- function(x) {
  if (is.character(x)) return(unclass(x))
  format(x, scientific = FALSE, trim = TRUE)
}

uuid_to_int64s <- function(u) {
  hex <- gsub("-", "", unclass(u), fixed = TRUE)
  bytes <- as.raw(strtoi(substring(hex, seq(1, 31, 2), seq(2, 32, 2)), 16L))
  list(integer_value(bytes_to_int64(bytes[1:8])), integer_value(bytes_to_int64(bytes[9:16])))
}

## Tags

decode_special_number <- function(x) {
  switch(x, "NaN" = NaN, "INF" = Inf, "-INF" = -Inf,
         stop("don't know how to decode special number: ", x, call. = FALSE))
}

decode_link <- function(rep) {
  get <- function(k) {
    v <- rep[[k]]
    if (is.null(v)) NULL else if (inherits(v, "transit_uri")) v else as.character(v)
  }
  transit_link(rep[["href"]], rep[["rel"]], name = get("name"), prompt = get("prompt"), render = get("render"))
}

split_cmap <- function(rep) {
  n <- length(rep)
  idx <- seq_len(n %/% 2)
  transit_map(rep[2 * idx - 1], rep[2 * idx])
}

# Decoders for each tag, given the tag's representation (a string for the
# one character tags of "~x..." strings, otherwise a decoded value).
DECODERS <- list(
  "_" = function(x) NULL,
  ":" = function(x) transit_keyword(x),
  "$" = function(x) transit_symbol(x),
  "?" = function(x) identical(x, "t") || isTRUE(x),
  "b" = function(x) jsonlite::base64_dec(x),
  "i" = function(x) if (is.character(x) && !inherits(x, c("transit_int64", "transit_bigint"))) integer_value(canonical_integer(x)) else x,
  "d" = function(x) if (is.character(x)) parse_doubles(x) else as.double(x),
  "f" = function(x) transit_decimal(x),
  "n" = function(x) transit_bigint(unclass(x)),
  "u" = decode_uuid,
  "r" = function(x) transit_uri(x),
  "t" = decode_date,
  "m" = decode_date,
  "z" = decode_special_number,
  "'" = function(x) x,
  "set" = function(x) transit_set(x),
  "list" = function(x) transit_list(x),
  "cmap" = split_cmap,
  "link" = decode_link
)

decode_tag <- function(tag, rep, st) {
  f <- st$handlers[[tag]]
  if (is.null(f)) f <- DECODERS[[tag]]
  if (is.null(f)) transit_tagged(tag, rep) else f(rep)
}

## Decoding

decode_node <- function(node, st, as_key = FALSE) {
  if (is.null(node)) return(NULL)
  if (inherits(node, c("transit_int64", "transit_bigint"))) return(node)
  if (is.character(node)) return(decode_string(node, st, as_key))
  if (inherits(node, "transit_wire_map")) return(decode_map(node$keys, node$values, st, as_key))
  if (is.list(node)) {
    if (!is.null(names(node))) return(decode_map(as.list(names(node)), unname(node), st, as_key))
    return(decode_array(node, st, as_key))
  }
  node
}

decode_array <- function(node, st, as_key) {
  n <- length(node)
  if (n == 0) return(list())
  first <- node[[1]]
  if (identical(first, "^ ")) {
    idx <- seq_len((n - 1) %/% 2)
    keys <- vector("list", length(idx))
    values <- vector("list", length(idx))
    for (i in idx) {
      keys[i] <- list(decode_node(node[[2 * i]], st, TRUE))
      values[i] <- list(decode_node(node[[2 * i + 1]], st, FALSE))
    }
    return(transit_map(keys, values))
  }
  # Each element must be decoded exactly once, in order, to keep the cache in
  # step with the writer.
  d1 <- decode_node(first, st, as_key)
  if (inherits(d1, "transit_tag_marker")) return(decode_tag(d1$tag, decode_node(node[[2]], st, FALSE), st))
  out <- vector("list", n)
  out[1] <- list(d1)
  for (i in seq_len(n)[-1]) out[i] <- list(decode_node(node[[i]], st, as_key))
  out
}

decode_map <- function(keys, values, st, as_key) {
  n <- length(keys)
  if (n == 1) {
    k <- decode_node(keys[[1]], st, TRUE)
    if (inherits(k, "transit_tag_marker")) return(decode_tag(k$tag, decode_node(values[[1]], st, FALSE), st))
    return(transit_map(list(k), list(decode_node(values[[1]], st, FALSE))))
  }
  dk <- vector("list", n)
  dv <- vector("list", n)
  for (i in seq_len(n)) {
    dk[i] <- list(decode_node(keys[[i]], st, TRUE))
    dv[i] <- list(decode_node(values[[i]], st, FALSE))
  }
  transit_map(dk, dv)
}

decode_string <- function(s, st, as_key) {
  s <- cache_read(st$cache, s, as_key)
  if (!startsWith(s, "~") || nchar(s) < 2) return(s)
  second <- substr(s, 2, 2)
  if (second == "#") return(tag_marker(substring(s, 3)))
  if (second %in% c("~", "^", "`")) return(substring(s, 2))
  decode_tag(second, substring(s, 3), st)
}

decode_value <- function(node, handlers = list()) {
  st <- new.env(parent = emptyenv())
  st$cache <- new_cache()
  st$handlers <- handlers
  decode_node(node, st)
}
