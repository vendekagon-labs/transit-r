# The compiled code reads and writes exactly what the R implementation does.

native_and_pure <- function(f) {
  native <- f()
  old <- options(transit.native = FALSE)
  on.exit(options(old))
  list(native = native, pure = f())
}

test_that("the compiled code is from transit-c", {
  expect_match(transit:::transit_c_version(), "^[0-9a-f]{7,}$")
})

test_that("reading and writing exemplars natively matches the R implementation", {
  skip_if_no_exemplars()
  names <- sub("[.]edn$", "", list.files(dirname(exemplar_path("x")), pattern = "[.]edn$"))
  for (name in names) {
    exts <- c(json = ".json", json_verbose = ".verbose.json", msgpack = ".mp")
    for (format in names(exts)) {
      ext <- exts[[format]]
      bytes <- read_bytes(exemplar_path(paste0(name, ext)))
      read <- native_and_pure(function() from_transit(bytes, format))
      expect_identical(read$native, read$pure, label = paste(name, format, "read"))
      for (out in c("json", "json_verbose", "msgpack")) {
        written <- native_and_pure(function() to_transit(read$pure, out))
        expect_identical(written$native, written$pure, label = paste(name, format, "written as", out))
      }
    }
  }
})

test_that("values the compiled code doesn't handle go to the R implementation", {
  expect_identical(to_transit(factor(c("x", "y"))), "[\"x\",\"y\"]")
  expect_identical(to_transit(as.POSIXlt(date_ms(946728000000))), "[\"~#'\",\"~m946728000000\"]")
  expect_identical(to_transit(as.Date(c("2000-01-01", "2000-01-02"))), "[\"~m946684800000\",\"~m946771200000\"]")
})
