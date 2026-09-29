# R representations of transit types R has no direct equivalent for. Each is
# a small S3 class so values roundtrip without losing their transit type.

string_class <- function(x, class) {
  x <- as.character(x)
  if (length(x) != 1 || is.na(x)) stop("expected a single string", call. = FALSE)
  structure(enc2utf8(x), class = class)
}

#' Transit value constructors
#'
#' Constructors for transit types that have no direct R equivalent. Most hold
#' a single string: `transit_int64()` a 64 bit integer outside R's integer
#' range, `transit_bigint()` an arbitrary precision integer and
#' `transit_decimal()` an arbitrary precision decimal, each in decimal form.
#'
#' @param x a string (or for `transit_int64()`, `transit_bigint()` and
#'   `transit_decimal()`, a number).
#' @name transit_types
NULL

#' @rdname transit_types
#' @export
transit_keyword <- function(x) string_class(x, "transit_keyword")

#' @rdname transit_types
#' @export
transit_symbol <- function(x) string_class(x, "transit_symbol")

#' @rdname transit_types
#' @export
transit_uuid <- function(x) {
  x <- tolower(as.character(x))
  if (!grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", x))
    stop("not a uuid: ", x, call. = FALSE)
  string_class(x, "transit_uuid")
}

#' @rdname transit_types
#' @export
transit_uri <- function(x) string_class(x, "transit_uri")

number_string <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, digits = 22, trim = TRUE)
  x
}

#' @rdname transit_types
#' @export
transit_decimal <- function(x) {
  if (is.numeric(x)) x <- sprintf("%.17g", x)
  string_class(x, "transit_decimal")
}

#' @rdname transit_types
#' @export
transit_bigint <- function(x) string_class(canonical_integer(number_string(x)), "transit_bigint")

#' @rdname transit_types
#' @export
transit_int64 <- function(x) {
  s <- canonical_integer(number_string(x))
  if (!int64_in_range(s)) stop("outside the 64 bit integer range: ", s, call. = FALSE)
  string_class(s, "transit_int64")
}

canonical_integer <- function(s) {
  s <- as.character(s)
  if (!grepl("^[-+]?[0-9]+$", s)) stop("not an integer: ", s, call. = FALSE)
  neg <- startsWith(s, "-")
  digits <- sub("^0+(?=.)", "", sub("^[-+]", "", s), perl = TRUE)
  if (neg && digits != "0") paste0("-", digits) else digits
}

#' Transit collections and tagged values
#'
#' `transit_map()` is a map whose keys can be any transit value (R named
#' lists only allow strings); `transit_set()` a set; `transit_list()` a
#' transit list, as distinct from an array (which is a plain R list).
#' `transit_tagged()` is a value with a tag this package doesn't interpret,
#' which is written back out unchanged. `transit_link()` is a transit link.
#'
#' @param keys,values lists of the same length.
#' @param elements a list.
#' @param tag a string.
#' @param rep the tagged value's representation, any transit value.
#' @param href,rel,name,prompt,render the parts of a link; `href` is a
#'   `transit_uri()` or a string.
#' @name transit_collections
NULL

#' @rdname transit_collections
#' @export
transit_map <- function(keys = list(), values = list()) {
  if (length(keys) != length(values)) stop("keys and values must have the same length", call. = FALSE)
  structure(list(keys = unname(as.list(keys)), values = unname(as.list(values))), class = "transit_map")
}

#' @rdname transit_collections
#' @export
transit_set <- function(elements = list()) structure(unname(as.list(elements)), class = "transit_set")

#' @rdname transit_collections
#' @export
transit_list <- function(elements = list()) structure(unname(as.list(elements)), class = "transit_list")

#' @rdname transit_collections
#' @export
transit_tagged <- function(tag, rep) {
  structure(list(tag = string_class(tag, NULL), rep = rep), class = "transit_tagged")
}

#' @rdname transit_collections
#' @export
transit_link <- function(href, rel, name = NULL, prompt = NULL, render = NULL) {
  if (!inherits(href, "transit_uri")) href <- transit_uri(href)
  structure(list(href = href, rel = as.character(rel), name = name, prompt = prompt, render = render),
            class = "transit_link")
}

map_keys <- function(m) unclass(m)$keys
map_values <- function(m) unclass(m)$values

#' @export
length.transit_map <- function(x) length(unclass(x)$keys)

#' Look up a key in a transit map
#'
#' `m[[key]]` finds the value for `key` (any transit value) in a
#' `transit_map()`, or `NULL` if it isn't there.
#'
#' @param x a `transit_map()`.
#' @param i the key.
#' @param ... unused.
#' @export
`[[.transit_map` <- function(x, i, ...) {
  j <- map_find(x, i)
  if (is.na(j)) NULL else map_values(x)[[j]]
}

map_find <- function(m, key) find_equal(key, map_keys(m))

# A string identifying a scalar value (its type and exact value), so that
# scalars can be matched without comparing them one by one; NA for anything
# else.
signature <- function(x) {
  if (is.null(x)) return("NULL")
  if (!is.atomic(x) || length(x) != 1 || !is.null(names(x))) return(NA_character_)
  value <- unclass(x)
  if (is.double(value)) value <- if (!is.na(value) && value == 0) "0" else sprintf("%.17g", value)
  paste0(paste(class(x), collapse = "/"), ":", typeof(x), ":", value)
}

# The index of the first element of candidates equal to x, or NA. Pass sigs
# (the candidates' signatures) when looking up many values.
find_equal <- function(x, candidates, sigs = vapply(candidates, signature, "")) {
  sig <- signature(x)
  if (!is.na(sig)) return(match(sig, sigs))
  for (j in which(is.na(sigs))) if (transit_equal(candidates[[j]], x)) return(j)
  NA_integer_
}

#' Convert a transit map to a named list
#'
#' Possible when every key is a string, keyword or symbol; keywords and
#' symbols lose their type.
#'
#' @param x a `transit_map()`.
#' @param ... unused.
#' @export
as.list.transit_map <- function(x, ...) {
  keys <- map_keys(x)
  ok <- vapply(keys, function(k) is.character(k) && length(k) == 1, logical(1))
  if (!all(ok)) stop("only maps with string, keyword or symbol keys convert to named lists", call. = FALSE)
  stats::setNames(map_values(x), vapply(keys, function(k) unclass(k), character(1)))
}

#' Compare transit values
#'
#' Like [identical()], except that maps and sets compare regardless of order
#' and `NaN` equals `NaN`.
#'
#' @param a,b values to compare.
#' @return `TRUE` or `FALSE`.
#' @export
transit_equal <- function(a, b) {
  if (inherits(a, "transit_map")) {
    if (!inherits(b, "transit_map") || length(a) != length(b)) return(FALSE)
    va <- map_values(a)
    vb <- map_values(b)
    ka <- map_keys(a)
    kb <- map_keys(b)
    sigs <- vapply(kb, signature, "")
    for (i in seq_along(ka)) {
      j <- find_equal(ka[[i]], kb, sigs)
      if (is.na(j) || !transit_equal(va[[i]], vb[[j]])) return(FALSE)
    }
    return(TRUE)
  }
  if (inherits(a, "transit_set")) {
    if (!inherits(b, "transit_set") || length(a) != length(b)) return(FALSE)
    eb <- unclass(b)
    sigs <- vapply(eb, signature, "")
    for (x in unclass(a)) if (is.na(find_equal(x, eb, sigs))) return(FALSE)
    return(TRUE)
  }
  if (is.list(a) && !is.data.frame(a)) {
    if (!is.list(b) || !identical(class(a), class(b)) || length(a) != length(b) ||
        !identical(names(a), names(b))) return(FALSE)
    for (i in seq_along(a)) if (!transit_equal(a[[i]], b[[i]])) return(FALSE)
    return(TRUE)
  }
  if (is.double(a) && is.double(b) && length(a) == 1 && length(b) == 1 && is.nan(a) && is.nan(b))
    return(identical(attributes(a), attributes(b)))
  identical(a, b)
}

## Printing, in an EDN-like notation.

#' Format transit values
#'
#' Renders a value in an EDN-like notation, as used by `print()` for the
#' transit classes.
#'
#' @param x a value.
#' @param ... unused.
#' @export
transit_format <- function(x, ...) {
  if (is.null(x)) return("nil")
  if (inherits(x, "transit_keyword")) return(paste0(":", unclass(x)))
  if (inherits(x, "transit_symbol")) return(unclass(x))
  if (inherits(x, "transit_uuid")) return(paste0("#uuid \"", unclass(x), "\""))
  if (inherits(x, "transit_uri")) return(paste0("#uri \"", unclass(x), "\""))
  if (inherits(x, c("transit_int64", "transit_bigint"))) return(unclass(x))
  if (inherits(x, "transit_decimal")) return(paste0(unclass(x), "M"))
  if (inherits(x, "transit_tagged")) return(paste0("#", x$tag, " ", transit_format(x$rep)))
  if (inherits(x, "transit_link")) return(paste0("#link ", transit_format(link_rep(x))))
  if (inherits(x, "transit_map")) {
    parts <- mapply(function(k, v) paste(transit_format(k), transit_format(v)), map_keys(x), map_values(x))
    return(paste0("{", paste(parts, collapse = ", "), "}"))
  }
  if (inherits(x, "transit_set")) return(paste0("#{", paste(vapply(unclass(x), transit_format, ""), collapse = " "), "}"))
  if (inherits(x, "transit_list")) return(paste0("(", paste(vapply(unclass(x), transit_format, ""), collapse = " "), ")"))
  if (inherits(x, c("POSIXct", "Date"))) return(paste0("#inst \"", format_rfc3339(date_to_ms(x)), "\""))
  if (is.raw(x)) return(paste0("#bytes \"", jsonlite::base64_enc(x), "\""))
  if (is.list(x) && is.null(names(x))) return(paste0("[", paste(vapply(x, transit_format, ""), collapse = " "), "]"))
  if (is.list(x) || (!is.null(names(x)) && is.atomic(x))) {
    parts <- mapply(function(k, v) paste(transit_format(k), transit_format(v)), names(x), as.list(x))
    return(paste0("{", paste(parts, collapse = ", "), "}"))
  }
  if (is.atomic(x) && length(x) != 1) return(paste0("[", paste(vapply(as.list(x), transit_format, ""), collapse = " "), "]"))
  if (is.character(x)) return(encodeString(x, quote = "\""))
  if (is.logical(x)) return(if (is.na(x)) "nil" else tolower(as.character(x)))
  if (is.double(x)) return(if (is.nan(x)) "##NaN" else if (is.infinite(x)) (if (x > 0) "##Inf" else "##-Inf") else format_double(x))
  if (is.integer(x)) return(if (is.na(x)) "nil" else as.character(x))
  paste(format(x), collapse = " ")
}

print_transit <- function(x, ...) {
  cat(transit_format(x), "\n", sep = "")
  invisible(x)
}

#' @export
print.transit_keyword <- print_transit
#' @export
print.transit_symbol <- print_transit
#' @export
print.transit_uuid <- print_transit
#' @export
print.transit_uri <- print_transit
#' @export
print.transit_int64 <- print_transit
#' @export
print.transit_bigint <- print_transit
#' @export
print.transit_decimal <- print_transit
#' @export
print.transit_map <- print_transit
#' @export
print.transit_set <- print_transit
#' @export
print.transit_list <- print_transit
#' @export
print.transit_tagged <- print_transit
#' @export
print.transit_link <- print_transit

#' @export
as.character.transit_int64 <- function(x, ...) unclass(x)
#' @export
as.character.transit_bigint <- function(x, ...) unclass(x)
#' @export
as.character.transit_decimal <- function(x, ...) unclass(x)
#' @export
as.numeric.transit_int64 <- function(x, ...) parse_doubles(unclass(x))
#' @export
as.numeric.transit_bigint <- function(x, ...) parse_doubles(unclass(x))
#' @export
as.numeric.transit_decimal <- function(x, ...) parse_doubles(unclass(x))
#' @export
as.double.transit_int64 <- function(x, ...) parse_doubles(unclass(x))
#' @export
as.double.transit_bigint <- function(x, ...) parse_doubles(unclass(x))
#' @export
as.double.transit_decimal <- function(x, ...) parse_doubles(unclass(x))
