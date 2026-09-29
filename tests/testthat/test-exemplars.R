# Every exemplar in transit-format: read in all three encodings, written and
# read back in all three, and written as msgpack byte for byte as
# transit-java does.

ints_centered_on <- function(m, n = 5) as.list(as.integer((m - n):(m + n)))

interesting <- function(k, d) {
  if (k <= 50) format(2^k + d, scientific = FALSE) else add_small(pow2_string(k), d)
}
INTERESTING_INTS <- unlist(lapply(0:65, function(k) vapply(-2:2, function(d) interesting(k, d), "")))

ARRAY_SIMPLE <- list(1L, 2L, 3L)
ARRAY_MIXED <- list(0L, 1L, 2.0, TRUE, FALSE, "five", kw("six"), sym("seven"), "~eight", NULL)
ARRAY_NESTED <- list(ARRAY_SIMPLE, ARRAY_MIXED)
SMALL_STRINGS <- list("", "a", "ab", "abc", "abcd", "abcde", "abcdef")
SYM_STRS <- c("a", "ab", "abc", "abcd", "abcde", "a1", "b2", "c3", "a_b")
UUIDS <- lapply(c("5a2cbea3-e8c6-428b-b525-21239370dd55", "d1dc64fa-da79-444b-9fa4-d4412f427289",
                  "501a978e-3a3e-4060-b3be-1cf2bd4b1a38", "b3ba141a-a776-48e4-9fae-a28ea8571f58"), transit_uuid)
URIS <- lapply(c("http://example.com", "ftp://example.com", "file:///path/to/file.txt",
                 "http://www.詹姆斯.com/"), transit_uri)

keyword_map <- function(names, values) transit_map(lapply(names, kw), values)
MAP_SIMPLE <- keyword_map(c("a", "b", "c"), list(1L, 2L, 3L))
MAP_MIXED <- keyword_map(c("a", "b", "c"), list(1L, "a string", TRUE))
MAP_NESTED <- keyword_map(c("simple", "mixed"), list(MAP_SIMPLE, MAP_MIXED))

key_names <- function(n) sprintf("key%04d", seq_len(n) - 1)
hash_of_size <- function(n) keyword_map(key_names(n), as.list(seq_len(n) - 1L))
keywords_repeated <- function(n) lapply(rep(key_names(n), 2), kw)
maps_with_keys <- function(k1, k2, f) lapply(list(c(1L, 2L), c(3L, 4L), c(5L, 6L)),
                                             function(v) transit_map(list(f(k1), f(k2)), list(v[1], v[2])))

EXEMPLARS <- list(
  nil = NULL,
  true = TRUE,
  false = FALSE,
  zero = 0L,
  one = 1L,
  one_string = "hello",
  one_keyword = kw("hello"),
  one_symbol = sym("hello"),
  one_date = date_ms(946728000000),
  vector_simple = ARRAY_SIMPLE,
  vector_empty = list(),
  vector_mixed = ARRAY_MIXED,
  vector_nested = ARRAY_NESTED,
  small_strings = SMALL_STRINGS,
  strings_tilde = lapply(SMALL_STRINGS, function(s) paste0("~", s)),
  strings_hash = lapply(SMALL_STRINGS, function(s) paste0("#", s)),
  strings_hat = lapply(SMALL_STRINGS, function(s) paste0("^", s)),
  ints = as.list(0:127),
  small_ints = ints_centered_on(0L),
  ints_interesting = lapply(INTERESTING_INTS, int_value),
  ints_interesting_neg = lapply(INTERESTING_INTS, function(s) int_value(negate(s))),
  doubles_small = as.list(as.double(-5:5)),
  doubles_interesting = list(-3.14159, 3.14159, 4E11, 2.998E8, 6.626E-34),
  one_uuid = UUIDS[[1]],
  uuids = UUIDS,
  one_uri = URIS[[1]],
  uris = URIS,
  dates_interesting = lapply(c(-6106017600000, 0, 946728000000, 1396909037000), date_ms),
  symbols = lapply(SYM_STRS, sym),
  keywords = lapply(SYM_STRS, kw),
  list_simple = transit_list(ARRAY_SIMPLE),
  list_empty = transit_list(),
  list_mixed = transit_list(ARRAY_MIXED),
  list_nested = transit_list(list(transit_list(ARRAY_SIMPLE), transit_list(ARRAY_MIXED))),
  set_simple = transit_set(ARRAY_SIMPLE),
  set_empty = transit_set(),
  set_mixed = transit_set(ARRAY_MIXED),
  set_nested = transit_set(list(transit_set(ARRAY_SIMPLE), transit_set(ARRAY_MIXED))),
  map_simple = MAP_SIMPLE,
  map_mixed = MAP_MIXED,
  map_nested = MAP_NESTED,
  map_string_keys = transit_map(list("first", "second", "third"), list(1L, 2L, 3L)),
  map_numeric_keys = transit_map(list(1L, 2L), list("one", "two")),
  map_vector_keys = transit_map(list(list(1L, 1L), list(2L, 2L)), list("one", "two")),
  map_unrecognized_vals = keyword_map("key", list("~Unrecognized")),
  vector_unrecognized_vals = list("~Unrecognized"),
  vector_1935_keywords_repeated_twice = keywords_repeated(1935),
  vector_1936_keywords_repeated_twice = keywords_repeated(1936),
  vector_1937_keywords_repeated_twice = keywords_repeated(1937),
  map_10_items = hash_of_size(10),
  map_10_nested = keyword_map(c("f", "s"), list(hash_of_size(10), hash_of_size(10))),
  map_1935_nested = keyword_map(c("f", "s"), list(hash_of_size(1935), hash_of_size(1935))),
  map_1936_nested = keyword_map(c("f", "s"), list(hash_of_size(1936), hash_of_size(1936))),
  map_1937_nested = keyword_map(c("f", "s"), list(hash_of_size(1937), hash_of_size(1937))),
  maps_two_char_sym_keys = maps_with_keys("aa", "bb", sym),
  maps_three_char_sym_keys = maps_with_keys("aaa", "bbb", sym),
  maps_four_char_sym_keys = maps_with_keys("aaaa", "bbbb", sym),
  maps_two_char_keyword_keys = maps_with_keys("aa", "bb", kw),
  maps_three_char_keyword_keys = maps_with_keys("aaa", "bbb", kw),
  maps_four_char_keyword_keys = maps_with_keys("aaaa", "bbbb", kw),
  maps_two_char_string_keys = maps_with_keys("aa", "bb", identity),
  maps_three_char_string_keys = maps_with_keys("aaa", "bbb", identity),
  maps_four_char_string_keys = maps_with_keys("aaaa", "bbbb", identity),
  maps_unrecognized_keys = list(transit_tagged("abcde", kw("anything")),
                                transit_tagged("fghij", kw("anything-else"))),
  vector_special_numbers = list(NaN, Inf, -Inf),
  cmap_null_key = transit_map(list(NULL, list(1L, 2L)), list("null as map key", "Array as key to force cmap")),
  cmap_pathological = list(
    keyword_map("any-value", list(transit_map(list(list("this vector makes this a cmap"), "any string"),
                                              list("any value", kw("victim"))))),
    keyword_map("victim", list(kw("any-other-value"))))
)

test_that("every transit-format exemplar is covered", {
  skip_if_no_exemplars()
  available <- sub("[.]edn$", "", list.files(dirname(exemplar_path("x")), pattern = "[.]edn$"))
  expect_setequal(names(EXEMPLARS), available)
})

for (name in names(EXEMPLARS)) {
  local({
    name <- name
    expected <- EXEMPLARS[[name]]
    test_that(paste("exemplar", name), {
      skip_if_no_exemplars()
      expect_transit_equal(read_transit(exemplar_path(paste0(name, ".json"))), expected, "json")
      expect_transit_equal(read_transit(exemplar_path(paste0(name, ".verbose.json")), "json_verbose"), expected, "verbose")
      mp <- read_bytes(exemplar_path(paste0(name, ".mp")))
      expect_transit_equal(from_transit(mp, "msgpack"), expected, "msgpack")
      for (format in c("json", "json_verbose", "msgpack")) {
        expect_transit_equal(from_transit(to_transit(expected, format), format), expected, paste("roundtrip", format))
      }
      # Map entries keep their order, so this is byte for byte what transit-java wrote.
      expect_identical(to_transit(from_transit(mp, "msgpack"), "msgpack"), mp)
    })
  })
}
