#' Convert values to and from transit
#'
#' `to_transit()` encodes a value as transit and `from_transit()` decodes
#' one. The format is `"json"` (the default), `"json_verbose"` or
#' `"msgpack"`; JSON is text (a string), msgpack is bytes (a raw vector).
#'
#' Reading gives: arrays as unnamed lists, maps as [transit_map()]s, integers
#' as integers (or [transit_int64()] beyond 32 bits), floats as doubles,
#' times as UTC `POSIXct`, bytes as raw vectors, and the other transit types
#' as the classes described in [transit_types] and [transit_collections].
#' Values with tags this package doesn't know, including characters and
#' ratios, are read as [transit_tagged()] values, which are written back out
#' unchanged.
#'
#' Writing takes those, and ordinary R values: `NULL` and `NA` are nil,
#' length one vectors are scalars, other atomic vectors and unnamed lists are
#' arrays, named lists and named vectors are maps with string keys, `Date`s
#' and `POSIXct`s are times.
#'
#' @param x the value to write, or for `from_transit()`, transit as a string
#'   or raw vector.
#' @param format `"json"`, `"json_verbose"` or `"msgpack"`.
#' @param handlers for writing, a named list of functions, by class, each
#'   turning an object of that class into a value that can be written (such
#'   as a [transit_tagged()]); for reading, a named list of functions, by
#'   tag, each turning a tag's representation into a value.
#' @return `to_transit()`: a string, or a raw vector for msgpack.
#'   `from_transit()`: the value.
#' @details Reading and writing use the package's compiled code (the
#' transit-c library), except when `handlers` are given, or for values it
#' doesn't handle (such as factors), which use the R implementation.
#' `options(transit.native = FALSE)` uses the R implementation throughout.
#' @examples
#' to_transit(list(1L, "two", transit_keyword("three")))
#' from_transit("[\"~#set\",[1,2]]")
#' @useDynLib transit, .registration = TRUE
#' @export
to_transit <- function(x, format = "json", handlers = list()) {
  format <- check_format(format)
  if (use_native(handlers)) {
    out <- .Call(C_to_transit, x, FORMAT_CODES[[format]])
    if (!is.null(out)) return(out)
  }
  st <- new_writer(format, handlers)
  out <- marshal_top(x, st)
  if (st$msgpack) out else enc2utf8(out)
}

#' @rdname to_transit
#' @export
from_transit <- function(x, format = "json", handlers = list()) {
  format <- check_format(format)
  if (use_native(handlers)) {
    if (is.character(x)) x <- enc2utf8(x)
    return(.Call(C_from_transit, x, FORMAT_CODES[[format]]))
  }
  if (format == "msgpack") {
    r <- mp_reader(bytes = x)
    return(decode_value(mp_unpack(r), handlers))
  }
  if (is.raw(x)) {
    x <- rawToChar(x)
    Encoding(x) <- "UTF-8"
  }
  decode_value(json_parse(x), handlers)
}

#' Read and write transit on connections
#'
#' `write_transit()` writes a value to a file or connection.
#' `read_transit()` reads one value: all of a file, or the next value on a
#' connection (it can be called repeatedly on the same connection).
#' `read_transit_stream()` calls a function with each value on a connection
#' as it arrives, until the end of the stream, which is how to process a
#' sequence of values from a pipe or socket.
#'
#' Connections are read in binary mode, and only as far as the end of each
#' value, so reading doesn't wait for data beyond the value being read.
#'
#' @inheritParams to_transit
#' @param x the value to write.
#' @param con a file path or connection.
#' @param callback a function called with each value.
#' @return `read_transit()`: the value. `read_transit_stream()`: the number of
#'   values read, invisibly. `write_transit()`: `x`, invisibly.
#' @export
write_transit <- function(x, con, format = "json", handlers = list()) {
  out <- to_transit(x, format, handlers)
  if (is.character(con)) {
    con <- file(con, "wb")
    on.exit(close(con))
  }
  if (!is.raw(out) && summary(con)$text == "text") cat(out, file = con)
  else writeBin(if (is.raw(out)) out else charToRaw(out), con)
  flush(con)
  invisible(x)
}

#' @rdname write_transit
#' @export
read_transit <- function(con, format = "json", handlers = list()) {
  format <- check_format(format)
  if (is.character(con)) {
    bytes <- readBin(con, "raw", file.size(con))
    return(from_transit(bytes, format, handlers))
  }
  value <- NULL
  n <- read_values(con, format, handlers, function(v) value <<- v, max = 1L)
  if (n == 0) stop("no transit value: at the end of the stream", call. = FALSE)
  value
}

#' @rdname write_transit
#' @export
read_transit_stream <- function(con, callback, format = "json", handlers = list()) {
  format <- check_format(format)
  if (is.character(con)) {
    con <- file(con, "rb")
    on.exit(close(con))
  }
  invisible(read_values(con, format, handlers, callback))
}

read_values <- function(con, format, handlers, callback, max = Inf) {
  if (!isOpen(con)) {
    open(con, "rb")
    on.exit(close(con))
  }
  n <- 0L
  if (format == "msgpack") {
    r <- mp_reader(con = con)
    while (n < max && !mp_at_end(r)) {
      if (use_native(handlers)) {
        start <- r$pos
        mp_skip(r)
        callback(from_transit(r$buf[start:(r$pos - 1L)], format))
      } else {
        callback(decode_value(mp_unpack(r), handlers))
      }
      n <- n + 1L
    }
  } else {
    while (n < max) {
      txt <- json_read_value(con)
      if (is.null(txt)) break
      callback(from_transit(txt, format, handlers))
      n <- n + 1L
    }
  }
  n
}

FORMAT_CODES <- c(json = 0L, json_verbose = 1L, msgpack = 2L)

use_native <- function(handlers) length(handlers) == 0 && !isFALSE(getOption("transit.native"))

# The transit-c commit the compiled code was built from.
transit_c_version <- function() .Call(C_transit_c_version)
