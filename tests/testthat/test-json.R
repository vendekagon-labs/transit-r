test_that("JSON numbers keep their type and exact value", {
  v <- from_transit("[3000000000,3000000000.0,1,1.0,9007199254740991,-0.0,1E3,\"~i9007199254740993\",\"~i-2147483648\"]")
  expect_identical(v[[1]], transit_int64("3000000000"))
  expect_identical(v[[2]], 3e9)
  expect_identical(v[[3]], 1L)
  expect_identical(v[[4]], 1.0)
  expect_identical(v[[5]], transit_int64("9007199254740991"))
  expect_identical(1 / v[[6]], -Inf)
  expect_identical(v[[7]], 1000)
  expect_identical(v[[8]], transit_int64("9007199254740993"))
  expect_identical(v[[9]], transit_int64("-2147483648"))
})

test_that("ints beyond 2^53 are written as strings in JSON", {
  expect_identical(to_transit(list(transit_int64("9007199254740991"), transit_int64("9007199254740992"),
                                   transit_int64("-9223372036854775808"))),
                   "[9007199254740991,\"~i9007199254740992\",\"~i-9223372036854775808\"]")
  expect_identical(to_transit(transit_bigint("123")), "[\"~#'\",\"~n123\"]")
})

test_that("doubles are written so they read back exactly, and as floats", {
  xs <- list(0.1, 1/3, 2, -0, 1e16, 6.626e-34, 4e11, 1.5e-5, .Machine$double.xmax, 2^-1074)
  out <- to_transit(xs)
  expect_identical(out, "[0.1,0.3333333333333333,2.0,-0.0,1e+16,6.626e-34,400000000000.0,1.5e-05,1.7976931348623157e+308,4.94065645841247e-324]")
  expect_identical(from_transit(out), xs)
})

test_that("doubles roundtrip exactly", {
  # R's as.numeric misreads many of these on some platforms (see parse_doubles)
  set.seed(1)
  xs <- c(runif(2000, -1e6, 1e6), exp(rnorm(2000, 0, 50)))
  for (format in c("json", "json_verbose", "msgpack")) {
    expect_identical(unlist(from_transit(to_transit(xs, format), format)), xs)
  }
  keys <- transit_map(as.list(xs[1:200]), as.list(seq_len(200)))
  expect_transit_equal(from_transit(to_transit(keys)), keys)
})

test_that("strings are escaped", {
  s <- "quote \" backslash \\ newline \n tab \t bell \a é \U0001f600"
  expect_identical(from_transit(to_transit(s)), s)
  expect_identical(to_transit(list("a\"b\\c\n\001")), "[\"a\\\"b\\\\c\\n\\u0001\"]")
  expect_identical(to_transit(list("~a", "^a", "`a", "^ ", "#a")), "[\"~~a\",\"~^a\",\"~`a\",\"~^ \",\"#a\"]")
  expect_identical(from_transit("[\"~~a\",\"~^a\",\"~`a\",\"~^ \"]"), list("~a", "^a", "`a", "^ "))
})

test_that("R values are written as transit", {
  expect_identical(to_transit(NULL), "[\"~#'\",null]")
  expect_identical(to_transit(NA), "[\"~#'\",null]")
  expect_identical(to_transit(c(1L, NA)), "[1,null]")
  expect_identical(to_transit(c(a = 1L, b = 2L)), "[\"^ \",\"a\",1,\"b\",2]")
  expect_identical(to_transit(list(a = 1L, b = list(2L))), "[\"^ \",\"a\",1,\"b\",[2]]")
  expect_identical(to_transit(stats::setNames(list(), character())), "[\"^ \"]")
  expect_identical(to_transit(list()), "[]")
  expect_identical(to_transit(character()), "[]")
  expect_identical(to_transit(factor(c("x", "y"))), "[\"x\",\"y\"]")
  expect_identical(to_transit(as.raw(c(0, 1, 255))), "[\"~#'\",\"~bAAH/\"]")
  expect_identical(to_transit(as.Date("2000-01-01")), "[\"~#'\",\"~m946684800000\"]")
  expect_identical(to_transit(date_ms(946728000123)), "[\"~#'\",\"~m946728000123\"]")
  expect_identical(to_transit(date_ms(946728000123), "json_verbose"), "{\"~#'\":\"~t2000-01-01T12:00:00.123Z\"}")
  expect_identical(to_transit(list(NaN, Inf, -Inf)), "[\"~zNaN\",\"~zINF\",\"~z-INF\"]")
  expect_error(to_transit(1i), "don't know how to write")
})

test_that("map keys", {
  m <- transit_map(list(NULL, TRUE, 1L, 1.5, kw("k"), transit_uuid("5a2cbea3-e8c6-428b-b525-21239370dd55")),
                   as.list(1:6))
  out <- to_transit(m)
  expect_identical(out, "[\"^ \",\"~_\",1,\"~?t\",2,\"~i1\",3,\"~d1.5\",4,\"~:k\",5,\"~u5a2cbea3-e8c6-428b-b525-21239370dd55\",6]")
  expect_transit_equal(from_transit(out), m)
  cm <- transit_map(list(list(1L), transit_set(list(1L))), list("a", "b"))
  expect_identical(to_transit(cm), "[\"~#cmap\",[[1],\"a\",[\"~#set\",[1]],\"b\"]]")
  expect_transit_equal(from_transit(to_transit(cm)), cm)
})

test_that("tagged values", {
  expect_identical(to_transit(transit_tagged("point", list(1L, 2L))), "[\"~#point\",[1,2]]")
  expect_identical(to_transit(transit_tagged("point", list(1L, 2L)), "json_verbose"), "{\"~#point\":[1,2]}")
  expect_identical(from_transit("[\"~xfoo\",[\"~#point\",[1,2]]]"),
                   list(transit_tagged("x", "foo"), transit_tagged("point", list(1L, 2L))))
  expect_identical(from_transit("[\"~ca\",[\"~#ratio\",[\"~n1\",\"~n3\"]]]"),
                   list(transit_tagged("c", "a"), transit_tagged("ratio", list(transit_bigint("1"), transit_bigint("3")))))
  expect_identical(to_transit(from_transit("[\"~ca\",[\"~#ratio\",[\"~n1\",\"~n3\"]]]")), "[\"~ca\",[\"~#ratio\",[\"~n1\",\"~n3\"]]]")
})

test_that("times", {
  expect_identical(from_transit("[\"~t2000-01-01T12:00:00.000Z\",\"~t2000-01-01T12:00:00Z\",\"~t2000-01-01T07:00:00-05:00\",\"~t2000-01-01T13:30:00.123456+01:30\",\"~m946728000000\"]"),
                   list(date_ms(946728000000), date_ms(946728000000), date_ms(946728000000),
                        date_ms(946728000123), date_ms(946728000000)))
})

test_that("links", {
  l <- transit_link("http://x.com", "self", name = "n")
  expect_identical(to_transit(l), "[\"~#link\",[\"^ \",\"href\",\"~rhttp://x.com\",\"rel\",\"self\",\"name\",\"n\"]]")
  expect_identical(from_transit(to_transit(l)), l)
})

test_that("custom handlers", {
  point <- function(x, y) structure(list(x = x, y = y), class = "point")
  write_point <- function(p) transit_tagged("point", list(p$x, p$y))
  read_point <- function(rep) point(rep[[1]], rep[[2]])
  out <- to_transit(list(point(1L, 2L)), handlers = list(point = write_point))
  expect_identical(out, "[[\"~#point\",[1,2]]]")
  expect_identical(from_transit(out, handlers = list(point = read_point)), list(point(1L, 2L)))
})

test_that("unsupported formats are an error", {
  expect_error(to_transit(1L, "yaml"), "format must be one of")
  expect_identical(to_transit(1L, "json-verbose"), "{\"~#'\":1}")
})
