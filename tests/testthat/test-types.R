test_that("64 bit integer conversions are exact", {
  cases <- c("0", "1", "-1", "127", "-128", "2147483647", "-2147483648", "9007199254740993",
             "-9007199254740993", "9223372036854775807", "-9223372036854775808")
  for (s in cases) {
    bytes <- transit:::int64_to_bytes(s)
    expect_length(bytes, 8)
    expect_identical(transit:::bytes_to_int64(bytes), s)
  }
  expect_identical(paste(as.character(transit:::int64_to_bytes("-1")), collapse = ""), "ffffffffffffffff")
  expect_identical(paste(as.character(transit:::int64_to_bytes("9223372036854775807")), collapse = ""), "7fffffffffffffff")
  expect_identical(transit:::bytes_to_int64(as.raw(rep(0xff, 8)), signed = FALSE), "18446744073709551615")
})

test_that("constructors", {
  expect_identical(transit_int64(3e9), transit_int64("3000000000"))
  expect_identical(transit_int64("+007"), transit_int64("7"))
  expect_error(transit_int64("9223372036854775808"), "outside")
  expect_error(transit_uuid("nope"), "not a uuid")
  expect_identical(transit_uuid("5A2CBEA3-E8C6-428B-B525-21239370DD55"), transit_uuid("5a2cbea3-e8c6-428b-b525-21239370dd55"))
  expect_error(transit_map(list(1), list()), "same length")
})

test_that("maps", {
  m <- transit_map(list("a", kw("b"), list(1L)), list(1L, 2L, 3L))
  expect_length(m, 3)
  expect_identical(m[["a"]], 1L)
  expect_identical(m[[kw("b")]], 2L)
  expect_identical(m[[list(1L)]], 3L)
  expect_null(m[["b"]])
  expect_identical(as.list(transit_map(list("a", kw("b")), list(1L, 2L))), list(a = 1L, b = 2L))
  expect_error(as.list(m), "only maps with string")
})

test_that("equality ignores map and set order", {
  expect_true(transit_equal(transit_map(list("a", "b"), list(1L, 2L)), transit_map(list("b", "a"), list(2L, 1L))))
  expect_false(transit_equal(transit_map(list("a"), list(1L)), transit_map(list("a"), list(2L))))
  expect_true(transit_equal(transit_set(list(1L, 2L)), transit_set(list(2L, 1L))))
  expect_false(transit_equal(transit_set(list(1L)), transit_list(list(1L))))
  expect_true(transit_equal(NaN, NaN))
  expect_false(transit_equal(1L, 1))
})

test_that("printing", {
  expect_identical(transit_format(transit_map(list(kw("a")), list(transit_set(list(1L, "x"))))), "{:a #{1 \"x\"}}")
  expect_identical(transit_format(list(transit_list(list(sym("s"))), NULL, TRUE, 1.5, transit_int64("3000000000"))),
                   "[(s) nil true 1.5 3000000000]")
  expect_output(print(kw("a")), ":a")
})
