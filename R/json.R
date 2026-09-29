# Reading JSON. jsonlite parses the structure, but reads every number as a
# double or 32 bit integer, losing whether 3000000000 was written as an
# integer and the exact value of integers beyond 2^53. So the numbers'
# source text is recovered, in document order, and used instead.

# Exact string to double conversion. R's own (as.numeric) isn't exact on
# platforms without extended precision long doubles, such as Apple Silicon,
# where it misreads a fifth of 17 digit doubles; jsonlite uses C's strtod.
parse_doubles <- function(text) {
  as.double(unlist(jsonlite::parse_json(paste0("[", paste(text, collapse = ","), "]"))))
}

JSON_TOKEN <- "\"(?:[^\"\\\\]++|\\\\.)*+\"|-?[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?"

json_parse <- function(txt) {
  node <- jsonlite::parse_json(txt, simplifyVector = FALSE)
  tokens <- regmatches(txt, gregexpr(JSON_TOKEN, txt, perl = TRUE))[[1]]
  numbers <- new.env(parent = emptyenv())
  numbers$text <- tokens[!startsWith(tokens, "\"")]
  numbers$i <- 0L
  exact_numbers(node, numbers)
}

exact_numbers <- function(node, numbers) {
  if (is.list(node)) {
    for (i in seq_along(node)) {
      v <- node[[i]]
      if (!is.null(v) && !is.character(v) && !is.logical(v)) node[i] <- list(exact_numbers(v, numbers))
    }
    return(node)
  }
  if (is.numeric(node)) {
    numbers$i <- numbers$i + 1L
    text <- numbers$text[numbers$i]
    if (grepl("[.eE]", text)) return(as.double(node))   # jsonlite's value is exact
    return(integer_value(canonical_integer(text)))
  }
  node
}

# Reads the text of the next JSON array, object or string from a binary
# connection, a byte at a time so as not to wait for data past its end.
# Returns NULL at the end of the stream.
json_read_value <- function(con) {
  buf <- raw(4096)
  n <- 0L
  depth <- 0L
  in_string <- FALSE
  escaped <- FALSE
  started <- FALSE
  repeat {
    b <- readBin(con, "raw", 1L)
    if (length(b) == 0) {
      if (started) stop(structure(class = c("transit_eof", "error", "condition"),
                                  list(message = "Stream ended in the middle of a JSON value", call = NULL)))
      return(NULL)
    }
    c <- as.integer(b)
    if (!started) {
      if (c %in% c(32L, 9L, 10L, 13L)) next
      started <- TRUE
      if (c == 91L || c == 123L) depth <- 1L          # [ {
      else if (c == 34L) in_string <- TRUE           # "
      else stop("expected a JSON array, object or string", call. = FALSE)
    } else if (in_string) {
      if (escaped) escaped <- FALSE
      else if (c == 92L) escaped <- TRUE             # backslash
      else if (c == 34L) in_string <- FALSE
    } else if (c == 34L) {
      in_string <- TRUE
    } else if (c == 91L || c == 123L) {
      depth <- depth + 1L
    } else if (c == 93L || c == 125L) {              # ] }
      depth <- depth - 1L
    }
    n <- n + 1L
    if (n > length(buf)) buf <- c(buf, raw(length(buf)))
    buf[n] <- b
    if (depth == 0L && !in_string) {
      txt <- rawToChar(buf[seq_len(n)])
      Encoding(txt) <- "UTF-8"
      return(txt)
    }
  }
}
