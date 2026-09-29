# Readers and writers in different languages must assign cache codes the
# same way, including when the cache fills up and starts over. R to R
# roundtrips can't show that, so these check against the payloads
# transit-clj produces, built here independently of the package.

CACHE_SIZE <- 44 * 44
N <- CACHE_SIZE + 1
KEYS <- sprintf("key%04d", 0:(N - 1))

# A map with one more cacheable key than the cache holds, followed by maps
# reusing the last and first keys.
wrapping_value <- function() {
  list(transit_map(lapply(KEYS, kw), as.list(0:(N - 1))),
       keyword_map1(KEYS[N], kw("last")),
       keyword_map1(KEYS[1], kw("first")),
       keyword_map1(KEYS[N], kw("last-again")))
}
keyword_map1 <- function(k, v) transit_map(list(kw(k)), list(v))

# What transit-clj writes: the last key of the big map is the first entry
# after the cache starts over, so it is "^0", while key0000 has been dropped
# and is written out again.
WRAPPING_JSON <- paste0(
  "[[\"^ \",", paste0("\"~:", KEYS, "\",", 0:(N - 1), collapse = ","), "],",
  "[\"^ \",\"^0\",\"~:last\"],[\"^ \",\"~:key0000\",\"~:first\"],[\"^ \",\"^0\",\"~:last-again\"]]")

# The same in msgpack, where maps are msgpack maps.
mp_fixstr <- function(s) c(as.raw(0xa0 + nchar(s)), charToRaw(s))
mp_small_int <- function(i) if (i <= 127) as.raw(i) else if (i <= 255) as.raw(c(0xcc, i)) else as.raw(c(0xcd, i %/% 256, i %% 256))
WRAPPING_MSGPACK <- c(
  as.raw(0x94),
  as.raw(c(0xde, N %/% 256, N %% 256)),
  unlist(lapply(0:(N - 1), function(i) c(mp_fixstr(paste0("~:", KEYS[i + 1])), mp_small_int(i)))),
  as.raw(0x81), mp_fixstr("^0"), mp_fixstr("~:last"),
  as.raw(0x81), mp_fixstr("~:key0000"), mp_fixstr("~:first"),
  as.raw(0x81), mp_fixstr("^0"), mp_fixstr("~:last-again"))

test_that("cache codes", {
  expect_equal(transit:::encode_key(0), "^0")
  expect_equal(transit:::encode_key(43), "^[")
  expect_equal(transit:::encode_key(44), "^10")
  expect_equal(transit:::encode_key(CACHE_SIZE - 1), "^[[")
  for (i in c(0, 1, 43, 44, 100, CACHE_SIZE - 1)) expect_equal(transit:::decode_key(transit:::encode_key(i)), i)
})

test_that("the cache starts over when full", {
  cache <- transit:::new_cache()
  for (i in 0:(CACHE_SIZE - 1)) transit:::cache_write(cache, paste0("~:k", i), FALSE)
  expect_equal(transit:::cache_write(cache, "~:k0", FALSE), "^0")
  expect_equal(transit:::cache_write(cache, "~:new", FALSE), "~:new")
  expect_equal(transit:::cache_write(cache, "~:new", FALSE), "^0")
  expect_equal(transit:::cache_write(cache, "~:k0", FALSE), "~:k0")
  expect_equal(transit:::cache_write(cache, "~:k0", FALSE), "^1")
})

test_that("an unknown cache code is an error", {
  expect_error(from_transit("[\"^0\"]"), "unknown cache code")
})

test_that("cache rollover matches transit-clj in JSON", {
  expect_identical(to_transit(wrapping_value()), WRAPPING_JSON)
  expect_transit_equal(from_transit(WRAPPING_JSON), wrapping_value())
})

test_that("cache rollover matches transit-clj in msgpack", {
  expect_identical(to_transit(wrapping_value(), "msgpack"), WRAPPING_MSGPACK)
  expect_transit_equal(from_transit(WRAPPING_MSGPACK, "msgpack"), wrapping_value())
})

test_that("the first element of an array is only cached once", {
  # It's inspected for a tag before the rest of the array is read.
  expect_transit_equal(from_transit("[[\"~:aaaa\",\"~:bbbb\"],\"^1\"]"),
                       list(list(kw("aaaa"), kw("bbbb")), kw("bbbb")))
})
