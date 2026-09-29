mp <- function(x) to_transit(list(x), "msgpack")
rt <- function(x) from_transit(to_transit(x, "msgpack"), "msgpack")

test_that("ints use the smallest encoding, as transit-java does", {
  cases <- list(
    list(0L, "00"), list(127L, "7f"), list(128L, "cc80"), list(255L, "ccff"), list(256L, "cd0100"),
    list(65535L, "cdffff"), list(65536L, "ce00010000"), list(transit_int64("4294967295"), "ceffffffff"),
    list(transit_int64("4294967296"), "cf0000000100000000"), list(-1L, "ff"), list(-32L, "e0"),
    list(-33L, "d0df"), list(-128L, "d080"), list(-129L, "d1ff7f"), list(-32768L, "d18000"),
    list(-32769L, "d2ffff7fff"), list(transit_int64("-2147483648"), "d280000000"),
    list(transit_int64("-2147483649"), "d3ffffffff7fffffff"),
    list(transit_int64("9223372036854775807"), "cf7fffffffffffffff"),
    list(transit_int64("-9223372036854775808"), "d38000000000000000"))
  for (case in cases) {
    bytes <- mp(case[[1]])
    expect_identical(paste(as.character(bytes[-1]), collapse = ""), case[[2]])
    expect_identical(from_transit(bytes, "msgpack"), list(case[[1]]))
  }
})

test_that("string, array and map sizes", {
  for (n in c(0, 31, 32, 255, 256, 65535, 65536)) {
    s <- strrep("x", n)
    expect_identical(rt(list(s)), list(s))
  }
  for (n in c(0, 15, 16, 65535, 65536)) {
    a <- as.list(seq_len(n))
    expect_identical(rt(a), a)
  }
  for (n in c(15, 16)) {
    m <- transit_map(as.list(paste0("k", seq_len(n))), as.list(seq_len(n)))
    expect_transit_equal(rt(m), m)
  }
})

test_that("msgpack values", {
  expect_identical(rt(list(1.5, -0, TRUE, FALSE, NULL, "é\U0001f600")), list(1.5, -0, TRUE, FALSE, NULL, "é\U0001f600"))
  u <- transit_uuid("5a2cbea3-e8c6-428b-b525-21239370dd55")
  expect_identical(rt(u), u)
  expect_identical(from_transit(to_transit(transit_map(list(1L), list("one")), "msgpack"), "msgpack"),
                   transit_map(list(1L), list("one")))
  # float32, bin and uint64 aren't written by transit writers, but are read
  expect_identical(from_transit(as.raw(c(0xca, 0x3f, 0xc0, 0, 0)), "msgpack"), 1.5)
  expect_identical(from_transit(as.raw(c(0x91, 0xc4, 0x02, 0x01, 0x02)), "msgpack"), list(as.raw(c(1, 2))))
  expect_identical(from_transit(as.raw(c(0xcf, rep(0xff, 8))), "msgpack"), transit_bigint("18446744073709551615"))
})

test_that("times and uuids are written as transit-java does", {
  skip_if_no_exemplars()
  expect_identical(to_transit(date_ms(946728000000), "msgpack"), read_bytes(exemplar_path("one_date.mp")))
  expect_identical(to_transit(transit_uuid("5a2cbea3-e8c6-428b-b525-21239370dd55"), "msgpack"),
                   read_bytes(exemplar_path("one_uuid.mp")))
})

test_that("msgpack errors", {
  expect_error(from_transit(as.raw(c(0xd4, 0x01, 0x00)), "msgpack"), "extension")
  expect_error(from_transit(as.raw(c(0x92, 0x01)), "msgpack"), "ended in the middle")
})
