# transit-format is expected at $TRANSIT_FORMAT_DIR, or checked out next to
# this repo.
transit_format_dir <- function() {
  dir <- Sys.getenv("TRANSIT_FORMAT_DIR")
  if (nzchar(dir)) return(dir)
  root <- testthat::test_path("..", "..")
  candidates <- c(file.path(root, "..", "transit-format"), file.path(root, "..", "..", "transit-format"))
  hit <- candidates[dir.exists(candidates)]
  if (length(hit)) normalizePath(hit[1]) else candidates[1]
}

exemplar_path <- function(name) file.path(transit_format_dir(), "examples", "0.8", "simple", name)

skip_if_no_exemplars <- function() {
  testthat::skip_if_not(dir.exists(dirname(exemplar_path("x"))), "transit-format exemplars not found")
}

read_bytes <- function(path) readBin(path, "raw", file.size(path))

read_text <- function(path) {
  txt <- rawToChar(read_bytes(path))
  Encoding(txt) <- "UTF-8"
  txt
}

expect_transit_equal <- function(actual, expected, label = NULL) {
  ok <- transit_equal(actual, expected)
  testthat::expect(ok, paste0(if (!is.null(label)) paste0(label, ": "), "expected ",
                              substr(transit_format(expected), 1, 300), "\n  got ",
                              substr(transit_format(actual), 1, 300)))
  invisible(actual)
}

kw <- function(x) transit_keyword(x)
sym <- function(x) transit_symbol(x)
date_ms <- function(ms) .POSIXct(ms / 1000, tz = "UTC")

# Exact decimal arithmetic on small integers, for building expected values.

# Digits of a non-negative integer string, least significant first.
lsd_digits <- function(s) rev(as.integer(strsplit(s, "")[[1]]))

digits_string <- function(d) {
  s <- paste(rev(d), collapse = "")
  sub("^0+(?=.)", "", s, perl = TRUE)
}

pow2_string <- function(k) {
  d <- 1L
  for (i in seq_len(k)) {
    d <- d * 2L
    carry <- 0L
    for (j in seq_along(d)) {
      v <- d[j] + carry
      d[j] <- v %% 10L
      carry <- v %/% 10L
    }
    if (carry > 0) d <- c(d, carry)
  }
  digits_string(d)
}

# s + d for a non-negative integer string s and a small integer d, where the
# result is non-negative.
add_small <- function(s, d) {
  x <- lsd_digits(s)
  x[1] <- x[1] + d
  for (j in seq_along(x)) {
    if (x[j] >= 10L) { x[j] <- x[j] - 10L; if (j == length(x)) x <- c(x, 0L); x[j + 1] <- x[j + 1] + 1L }
    if (x[j] < 0L) { x[j] <- x[j] + 10L; x[j + 1] <- x[j + 1] - 1L }
  }
  digits_string(x)
}

# a <= b for non-negative integer strings.
mag_le <- function(a, b) {
  if (nchar(a) != nchar(b)) return(nchar(a) < nchar(b))
  x <- rev(lsd_digits(a))
  y <- rev(lsd_digits(b))
  i <- which(x != y)
  length(i) == 0 || x[i[1]] < y[i[1]]
}

# The value transit gives integer s: an integer, or beyond 32 bits a
# transit_int64, or beyond 64 bits a transit_bigint.
int_value <- function(s) {
  neg <- startsWith(s, "-")
  mag <- sub("^-", "", s)
  if (mag_le(mag, "2147483647")) return(as.integer(s))
  if (mag_le(mag, if (neg) "9223372036854775808" else "9223372036854775807")) return(transit_int64(s))
  transit_bigint(s)
}

negate <- function(s) if (s == "0") "0" else if (startsWith(s, "-")) substring(s, 2) else paste0("-", s)
