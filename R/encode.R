# Turns R values into transit, as JSON text or msgpack bytes.
#
# Each value is classified as a tag and representation (as in the other
# transit implementations); ground types are written directly, values with
# one character tags as "~x..." strings where the encoding prefers strings,
# and everything else as tagged values. Values are encoded strictly in
# document order, which keeps the cache in step with readers.

FORMATS <- c("json", "json_verbose", "msgpack")

check_format <- function(format) {
  format <- gsub("-", "_", format, fixed = TRUE)
  if (length(format) != 1 || !(format %in% FORMATS))
    stop("format must be one of: ", paste(FORMATS, collapse = ", "), call. = FALSE)
  format
}

new_writer <- function(format, handlers) {
  st <- new.env(parent = emptyenv())
  st$format <- format
  st$verbose <- format == "json_verbose"
  st$msgpack <- format == "msgpack"
  st$cache <- new_cache(enabled = !st$verbose)
  st$handlers <- handlers
  st
}

is_plain_string <- function(x) is.character(x) && length(x) == 1 && is.null(attr(x, "class"))

format_double <- function(x) {
  s <- sprintf("%.15g", x)
  if (parse_doubles(s) != x) s <- sprintf("%.17g", x)
  if (!grepl("[.e]", s)) s <- paste0(s, ".0")
  s
}

link_rep <- function(l) {
  keys <- list("href", "rel")
  values <- list(l$href, l$rel)
  for (k in c("name", "prompt", "render")) {
    if (!is.null(l[[k]])) {
      keys <- c(keys, list(k))
      values <- c(values, list(l[[k]]))
    }
  }
  transit_map(keys, values)
}

ms_int <- function(ms) integer_value(format(ms, scientific = FALSE, trim = TRUE))

# The tag and representation of x: list(tag, rep, str), where str is the
# string representation used for "~x" strings (when rep isn't a string), and
# maps have keys and values instead of rep.
classify <- function(x, st) {
  for (cl in class(x)) {
    h <- st$handlers[[cl]]
    if (!is.null(h)) return(classify(h(x), st))
  }
  if (is.null(x)) return(list(tag = "_"))
  if (inherits(x, "transit_keyword")) return(list(tag = ":", rep = unclass(x)))
  if (inherits(x, "transit_symbol")) return(list(tag = "$", rep = unclass(x)))
  if (inherits(x, "transit_uuid")) return(list(tag = "u", rep = uuid_to_int64s(x), str = unclass(x)))
  if (inherits(x, "transit_uri")) return(list(tag = "r", rep = unclass(x)))
  if (inherits(x, "transit_decimal")) return(list(tag = "f", rep = unclass(x)))
  if (inherits(x, "transit_bigint")) return(list(tag = "n", rep = unclass(x)))
  if (inherits(x, "transit_int64")) return(list(tag = "i", rep = x))
  if (inherits(x, "transit_tagged")) return(list(tag = x$tag, rep = x$rep, str = if (is_plain_string(x$rep)) x$rep))
  if (inherits(x, "transit_map")) return(list(tag = "map", keys = map_keys(x), values = map_values(x)))
  if (inherits(x, "transit_set")) return(list(tag = "set", rep = unclass(x)))
  if (inherits(x, "transit_list")) return(list(tag = "list", rep = unclass(x)))
  if (inherits(x, "transit_link")) return(list(tag = "link", rep = link_rep(x)))
  if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
    if (length(x) != 1) return(list(tag = "array", rep = lapply(seq_along(x), function(i) x[i])))
    if (is.na(x)) return(list(tag = "_"))
    ms <- date_to_ms(x)
    if (st$verbose) return(list(tag = "t", rep = format_rfc3339(ms)))
    return(list(tag = "m", rep = ms_int(ms), str = format(ms, scientific = FALSE, trim = TRUE)))
  }
  if (is.raw(x)) return(list(tag = "b", rep = jsonlite::base64_enc(x)))
  if (is.factor(x)) return(classify(stats::setNames(as.character(x), names(x)), st))
  if (is.list(x)) {
    if (!is.null(names(x))) return(list(tag = "map", keys = as.list(names(x)), values = unname(as.list(x))))
    return(list(tag = "array", rep = unclass(x)))
  }
  if (is.atomic(x) && !is.complex(x)) {
    if (!is.null(names(x))) return(list(tag = "map", keys = as.list(names(x)), values = unname(as.list(x))))
    if (length(x) != 1) return(list(tag = "array", rep = as.list(unclass(x))))
    if (is.double(x)) {
      if (is.nan(x)) return(list(tag = "z", rep = "NaN"))
      if (is.infinite(x)) return(list(tag = "z", rep = if (x > 0) "INF" else "-INF"))
    }
    if (is.na(x)) return(list(tag = "_"))
    if (is.logical(x)) return(list(tag = "?", rep = unclass(x)))
    if (is.integer(x)) return(list(tag = "i", rep = unclass(x)))
    if (is.double(x)) return(list(tag = "d", rep = unclass(x)))
    if (is.character(x)) return(list(tag = "s", rep = enc2utf8(unclass(x))))
  }
  stop("don't know how to write a value of class ", paste(class(x), collapse = "/"), call. = FALSE)
}

escape_string <- function(s) {
  if (nzchar(s) && substr(s, 1, 1) %in% c("~", "^", "`")) paste0("~", s) else s
}

int_str <- function(v) if (is.integer(v)) as.character(v) else unclass(v)

## Emitters: the JSON ones return JSON text, the msgpack ones raw bytes.

emit_str <- function(st, s, as_key) {
  s <- cache_write(st$cache, s, as_key)
  if (st$msgpack) mp_str(s) else json_quote(s)
}

emit_null <- function(st) if (st$msgpack) as.raw(0xc0) else "null"

emit_bool <- function(st, b) {
  if (st$msgpack) as.raw(if (b) 0xc3 else 0xc2) else if (b) "true" else "false"
}

emit_float <- function(st, x) if (st$msgpack) mp_float(x) else format_double(x)

emit_int <- function(st, v, as_key) {
  s <- int_str(v)
  if (as_key) return(emit_str(st, paste0("~i", s), TRUE))
  if (st$msgpack) return(mp_int(s))
  if (json_safe_integer(s)) s else emit_str(st, paste0("~i", s), FALSE)
}

emit_array <- function(st, pieces) {
  if (st$msgpack) c(mp_header(length(pieces), 0x90, 0xdc, 0xdd), unlist(pieces, use.names = FALSE))
  else paste0("[", paste(unlist(pieces, use.names = FALSE), collapse = ","), "]")
}

emit_tagged <- function(st, tag, rep) {
  tag_piece <- emit_str(st, paste0("~#", tag), FALSE)   # before the rep, for the cache
  rep_piece <- marshal(rep, FALSE, st)
  if (st$msgpack) c(as.raw(0x92), tag_piece, rep_piece)
  else if (st$verbose) paste0("{", tag_piece, ":", rep_piece, "}")
  else paste0("[", tag_piece, ",", rep_piece, "]")
}

## Marshaling

marshal <- function(x, as_key, st) {
  h <- classify(x, st)
  # msgpack map keys can be any type, so like transit-java, nil, booleans,
  # ints and floats are written as themselves there, not as strings.
  if (as_key && st$msgpack && h$tag %in% c("_", "?", "i", "d")) as_key <- FALSE
  switch(h$tag,
    "_" = if (as_key) emit_str(st, "~_", TRUE) else emit_null(st),
    "?" = if (as_key) emit_str(st, if (h$rep) "~?t" else "~?f", TRUE) else emit_bool(st, h$rep),
    "s" = emit_str(st, escape_string(h$rep), as_key),
    "i" = emit_int(st, h$rep, as_key),
    "d" = if (as_key) emit_str(st, paste0("~d", format_double(h$rep)), TRUE) else emit_float(st, h$rep),
    "array" = emit_array(st, lapply(h$rep, marshal, FALSE, st)),
    "map" = marshal_map(h$keys, h$values, st),
    marshal_encoded(h, as_key, st))
}

marshal_encoded <- function(h, as_key, st) {
  tag <- h$tag
  if (nchar(tag) == 1) {
    if (is_plain_string(h$rep)) return(emit_str(st, paste0("~", tag, h$rep), as_key))
    if (!is.null(h$str) && (as_key || !st$msgpack)) return(emit_str(st, paste0("~", tag, h$str), as_key))
  }
  if (as_key) stop("a value with tag '", tag, "' can't be written as a map key", call. = FALSE)
  emit_tagged(st, tag, h$rep)
}

is_stringable <- function(k, st) nchar(classify(k, st)$tag) == 1

marshal_map <- function(keys, values, st) {
  n <- length(keys)
  if (!all(vapply(keys, is_stringable, logical(1), st = st))) {
    flat <- vector("list", 2 * n)
    flat[2 * seq_len(n) - 1] <- keys
    flat[2 * seq_len(n)] <- values
    return(emit_tagged(st, "cmap", flat))
  }
  pieces <- vector("list", 2 * n)
  for (i in seq_len(n)) {
    pieces[[2 * i - 1]] <- marshal(keys[[i]], TRUE, st)
    pieces[[2 * i]] <- marshal(values[[i]], FALSE, st)
  }
  if (st$msgpack) return(c(mp_header(n, 0x80, 0xde, 0xdf), unlist(pieces, use.names = FALSE)))
  if (st$verbose) {
    if (n == 0) return("{}")
    kv <- paste0(unlist(pieces[2 * seq_len(n) - 1]), ":", unlist(pieces[2 * seq_len(n)]))
    return(paste0("{", paste(kv, collapse = ","), "}"))
  }
  paste0("[", paste(c("\"^ \"", unlist(pieces, use.names = FALSE)), collapse = ","), "]")
}

marshal_top <- function(x, st) {
  h <- classify(x, st)
  if (nchar(h$tag) == 1) emit_tagged(st, "'", x) else marshal(x, FALSE, st)
}

## JSON text

json_quote <- function(s) {
  s <- enc2utf8(s)
  cps <- utf8ToInt(s)
  if (any(cps < 32L | cps == 34L | cps == 92L, na.rm = TRUE)) {
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    s <- gsub("\"", "\\\"", s, fixed = TRUE)
    for (cp in sort(unique(cps[cps < 32L]))) {
      esc <- switch(as.character(cp), "8" = "\\b", "9" = "\\t", "10" = "\\n", "12" = "\\f", "13" = "\\r",
                    sprintf("\\u%04x", cp))
      s <- gsub(intToUtf8(cp), esc, s, fixed = TRUE)
    }
  }
  paste0("\"", s, "\"")
}
