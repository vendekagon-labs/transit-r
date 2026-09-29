# The transit cache. Readers and writers must assign codes the same way, so
# this follows the spec (and transit-java): codes are assigned in order from
# "^0", and once CACHE_SIZE entries are in use the cache starts over.

FIRST_ORD <- 48L
CACHE_CODE_DIGITS <- 44L
CACHE_SIZE <- CACHE_CODE_DIGITS * CACHE_CODE_DIGITS
MIN_SIZE_CACHEABLE <- 4L

new_cache <- function(enabled = TRUE) {
  cache <- new.env(parent = emptyenv())
  cache$enabled <- enabled
  cache_clear(cache)
  cache
}

cache_clear <- function(cache) {
  cache$index <- 0L
  cache$values <- character(CACHE_SIZE)
  cache$codes <- new.env(hash = TRUE, parent = emptyenv())
}

encode_key <- function(i) {
  hi <- i %/% CACHE_CODE_DIGITS
  lo <- i %% CACHE_CODE_DIGITS
  if (hi == 0) paste0("^", intToUtf8(lo + FIRST_ORD))
  else paste0("^", intToUtf8(hi + FIRST_ORD), intToUtf8(lo + FIRST_ORD))
}

decode_key <- function(s) {
  cp <- utf8ToInt(s)
  if (length(cp) == 2) cp[2] - FIRST_ORD
  else (cp[3] - FIRST_ORD) + CACHE_CODE_DIGITS * (cp[2] - FIRST_ORD)
}

is_cache_key <- function(s) nchar(s) > 0 && startsWith(s, "^") && s != "^ "

is_cacheable <- function(s, as_key) {
  nchar(s) >= MIN_SIZE_CACHEABLE &&
    (as_key || substr(s, 1, 2) %in% c("~#", "~$", "~:"))
}

cache_add <- function(cache, s) {
  if (cache$index >= CACHE_SIZE) cache_clear(cache)
  i <- cache$index
  cache$values[i + 1L] <- s
  assign(s, encode_key(i), envir = cache$codes)
  cache$index <- i + 1L
  invisible(s)
}

# Reading: the string a code stands for, or s itself (remembered if cacheable).
cache_read <- function(cache, s, as_key) {
  if (is_cache_key(s)) {
    i <- decode_key(s)
    if (is.na(i) || i < 0 || i >= cache$index) stop("unknown cache code: ", s, call. = FALSE)
    return(cache$values[i + 1L])
  }
  if (is_cacheable(s, as_key)) cache_add(cache, s)
  s
}

# Writing: the code for s if it has one, otherwise s (remembered if cacheable).
cache_write <- function(cache, s, as_key) {
  if (!cache$enabled || !is_cacheable(s, as_key)) return(s)
  code <- get0(s, envir = cache$codes, inherits = FALSE)
  if (!is.null(code)) return(code)
  cache_add(cache, s)
  s
}
