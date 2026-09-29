# Exact conversions between decimal strings and 64 bit two's complement bytes.
# R has no 64 bit integer type and doubles are only exact to 2^53, so this
# works on decimal digits and 16 bit limbs.

INT64_MAX <- "9223372036854775807"
INT64_MIN_MAGNITUDE <- "9223372036854775808"
JSON_MAX_MAGNITUDE <- "9007199254740991"   # 2^53 - 1

# a <= b for digit strings without signs or leading zeros
magnitude_le <- function(a, b) {
  if (nchar(a) != nchar(b)) return(nchar(a) < nchar(b))
  da <- utf8ToInt(a)
  db <- utf8ToInt(b)
  diff <- which(da != db)
  length(diff) == 0 || da[diff[1]] < db[diff[1]]
}

# For canonical integer strings (as made by canonical_integer()).
int_magnitude <- function(s) sub("^-", "", s)

int64_in_range <- function(s) {
  magnitude_le(int_magnitude(s), if (startsWith(s, "-")) INT64_MIN_MAGNITUDE else INT64_MAX)
}

int32_in_range <- function(s) {
  magnitude_le(int_magnitude(s), if (startsWith(s, "-")) "2147483647" else "2147483647")
}

json_safe_integer <- function(s) magnitude_le(int_magnitude(s), JSON_MAX_MAGNITUDE)

# The integer value of canonical string s: an R integer if it fits in 32
# bits (not NA, which is -2^31), otherwise a transit_int64 (or, beyond 64
# bits, a transit_bigint).
integer_value <- function(s) {
  if (int32_in_range(s)) return(as.integer(s))
  if (int64_in_range(s)) return(structure(s, class = "transit_int64"))
  structure(s, class = "transit_bigint")
}

# Little endian 16 bit limbs of the magnitude of canonical string s.
magnitude_limbs <- function(s, n = 4) {
  digits <- utf8ToInt(int_magnitude(s)) - 48L
  limbs <- numeric(n)
  for (k in seq_len(n)) {
    r <- 0
    for (i in seq_along(digits)) {
      cur <- r * 10 + digits[i]
      digits[i] <- cur %/% 65536
      r <- cur %% 65536
    }
    limbs[k] <- r
  }
  limbs
}

negate_limbs <- function(limbs) {
  limbs <- 65535 - limbs
  carry <- 1
  for (k in seq_along(limbs)) {
    v <- limbs[k] + carry
    limbs[k] <- v %% 65536
    carry <- v %/% 65536
  }
  limbs
}

# 8 big endian bytes, two's complement, of canonical integer string s.
int64_to_bytes <- function(s) {
  limbs <- magnitude_limbs(s)
  if (startsWith(s, "-")) limbs <- negate_limbs(limbs)
  limbs <- rev(limbs)
  as.raw(as.vector(rbind(limbs %/% 256, limbs %% 256)))
}

# Canonical decimal string of 8 big endian bytes.
bytes_to_int64 <- function(bytes, signed = TRUE) {
  b <- as.integer(bytes)
  limbs <- rev(b[c(1, 3, 5, 7)] * 256 + b[c(2, 4, 6, 8)])
  neg <- signed && b[1] >= 128
  if (neg) limbs <- negate_limbs(limbs)
  digits <- 0
  for (limb in rev(limbs)) {
    carry <- limb
    for (i in rev(seq_along(digits))) {
      v <- digits[i] * 65536 + carry
      digits[i] <- v %% 10
      carry <- v %/% 10
    }
    while (carry > 0) {
      digits <- c(carry %% 10, digits)
      carry <- carry %/% 10
    }
  }
  s <- sub("^0+(?=.)", "", paste(digits, collapse = ""), perl = TRUE)
  if (neg && s != "0") paste0("-", s) else s
}
