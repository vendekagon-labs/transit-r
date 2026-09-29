# Reading a sequence of values from a connection as the data arrives.

VALUES <- list(1L, "~tilde", list(kw("abcd"), kw("abcd")),
               transit_map(list(kw("abcd")), list("quote \" and backslash \\ é \U0001f600")),
               transit_set(list(1L, 2L)), NULL)

written <- function(values, format) {
  pieces <- lapply(values, function(v) {
    out <- to_transit(v, format)
    if (is.raw(out)) out else charToRaw(out)
  })
  unlist(pieces)
}

read_all <- function(bytes, format) {
  con <- rawConnection(bytes, "rb")
  on.exit(close(con))
  values <- list()
  n <- read_transit_stream(con, function(v) values[length(values) + 1] <<- list(v), format)
  expect_equal(n, length(values))
  values
}

test_that("a stream of values is read one at a time", {
  for (format in c("json", "json_verbose", "msgpack")) {
    expect_transit_equal(read_all(written(VALUES, format), format), VALUES, format)
  }
  expect_identical(read_all(charToRaw(" [\"~#'\",1]\n\t{\"~#'\":2}  \n"), "json"), list(1L, 2L))
  expect_identical(read_all(raw(), "json"), list())
  expect_identical(read_all(raw(), "msgpack"), list())
})

test_that("read_transit reads the next value on a connection", {
  for (format in c("json", "msgpack")) {
    con <- rawConnection(written(list(1L, 2L), format), "rb")
    expect_identical(read_transit(con, format), 1L)
    expect_identical(read_transit(con, format), 2L)
    expect_error(read_transit(con, format), "end of the stream")
    close(con)
  }
})

test_that("a stream ending part way through a value is an error", {
  expect_error(read_all(charToRaw("[1,\"a]"), "json"), "ended in the middle")
  expect_error(read_all(as.raw(c(0x92, 0x01)), "msgpack"), "ended in the middle")
})

test_that("files", {
  path <- tempfile()
  on.exit(unlink(path))
  for (format in c("json", "json_verbose", "msgpack")) {
    write_transit(VALUES, path, format)
    expect_transit_equal(read_transit(path, format), VALUES, format)
  }
})

test_that("bin/roundtrip, which transit-format's verify harness drives", {
  roundtrip <- normalizePath(test_path("..", "..", "bin", "roundtrip"), mustWork = FALSE)
  skip_if_not(file.exists(roundtrip), "not running from a source checkout")
  skip_on_os("windows")
  for (format in c("json", "json_verbose", "msgpack")) {
    input <- tempfile()
    writeBin(written(VALUES, format), input)
    out <- system2(roundtrip, gsub("_", "-", format), stdin = input, stdout = TRUE, stderr = FALSE)
    unlink(input)
    if (format != "msgpack") expect_identical(paste(out, collapse = "\n"), rawToChar(written(VALUES, format)))
  }
  # msgpack output is binary, so compare it through a file
  input <- tempfile()
  output <- tempfile()
  writeBin(written(VALUES, "msgpack"), input)
  system2(roundtrip, "msgpack", stdin = input, stdout = output, stderr = FALSE)
  expect_identical(read_bytes(output), written(VALUES, "msgpack"))
  unlink(c(input, output))
})
