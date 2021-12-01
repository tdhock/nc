library(nc)
library(testthat)
context("errors")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

foo.bar <- c("foo", "bar")
test_engines("no capture groups is an error", {
  expect_error({
    capture_first_vec(foo.bar, "o")
  }, "must have at least one named argument", fixed=TRUE)
})

named.chr.vec <- c("v\"name\\"="[a\"o]+")
expected.obj <- as.list(named.chr.vec)
expect_error_with_code <- function(expr, expected.obj){
  msg <- tryCatch({
    expr
  }, error=function(e){
    e$message
  })
  code <- capture_first_vec(
    msg,
    "did you mean ",
    code=".*")$code
  computed.obj <- eval(parse(text=code))
  expect_identical(computed.obj, expected.obj)
}
test_engines("named character vector is an error with group name", {
  expect_error_with_code({
    capture_first_vec(foo.bar, gname=named.chr.vec)
  }, expected.obj)
})
test_engines("named character vector is an error without group name", {
  expect_error_with_code({
    capture_first_vec(foo.bar, named.chr.vec)
  }, expected.obj)
})

named.pat.list <- as.list(named.chr.vec)
exp.vec <- c("oo", "a")
test_engines("named pattern list in named arg makes two groups", {
  (result.df <- capture_first_vec(foo.bar, gname=named.pat.list))
  expect_identical(names(result.df), c("gname", names(named.pat.list)))
  expect_identical(result.df$gname, exp.vec)
  expect_identical(result.df[[names(named.pat.list)]], exp.vec)
})
test_engines("named pattern list in un-named arg makes one group", {
  (result.df <- capture_first_vec(foo.bar, named.pat.list))
  expect_identical(names(result.df), names(named.pat.list))
  expect_identical(result.df[[names(named.pat.list)]], exp.vec)
})

test_engines("any capture group without a name is an error", {
  expect_error({
    capture_first_vec(foo.bar, "(o)(?P<name>o)")
  }, "must have at least one named argument", fixed=TRUE)
})

test_engines("NA pattern is an error", {
  expect_error({
    capture_first_vec(foo.bar, NA_character_)
  }, "patterns must not be missing/NA", fixed=TRUE)
})

test_engines("factor pattern is an error", {
  expect_error({
    capture_first_vec(foo.bar, factor("(?P<regex>foo)"))
  }, "arguments must be character")
})

test_engines("multiple patterns is an error", {
  expect_error({
    capture_first_vec(foo.bar, c("(?P<name>.)", "(?P<name>.)"))
  }, "patterns must be character vectors of length 1")
})

test_engines("subject of length 0 is an error", {
  expect_error({
    capture_first_vec(character(), "(?P<name>.)")
  }, "subject has class=character and length=0 but should be a character vector with length>0")
})

test_engines("capture all works with only one 'name' group", {
  subject <- c(missing=NA, nomatch="", match="foobar")
  result.dt <- capture_all_str(subject, name="foo")
  expect_equal(dim(result.dt), c(1, 1))
  expect_identical(result.dt$name, "foo")
})

test_engines("informative error when converter fun has zero args", {
  expect_error({
    capture_first_vec(
      "chr2:300-400",
      chrom="chr", function()y)
  }, "atomic vector",
  fixed=TRUE)
})

test_engines("informative error when converter returns wrong length", {
  expect_error({
    capture_first_vec(
      c("chr2:300-400", "chr2:300-400"),
      chrom="chr", function(x)"foo")
  }, "type conversion function for group 1(chrom) returned vector of length 1 but expected length 2", fixed=TRUE)
})

test_engines("informative error when converter returns non-atomic", {
  expect_error({
    capture_first_vec(
      c("chr2:300-400", "chr2:300-400"),
      chrom="chr", function(x)list(foo=200))
  }, "each type conversion function must return an atomic vector, but function for group 1(chrom) did not", fixed=TRUE)
})

test_engines("error for name group, missing subject, nomatch.error=TRUE", {
  expect_error({
    capture_first_vec(
      c("chr1:20-40", NA, "chr2:300-400"),
      name="[^:]+",
      ":",
      chromStart="[0-9]+",
      nomatch.error=TRUE)
  }, "subject(s) 2 (1 total) did not match regex below", fixed=TRUE)
})

test_engines("error for name group, no match, nomatch.error=TRUE", {
  expect_error({
    capture_first_vec(
      c("chr1:20-40", "foobar", "chr2:300-400"),
      name="[^:]+",
      ":",
      chromStart="[0-9]+",
      nomatch.error=TRUE)
  }, "subject(s) 2 (1 total) did not match regex below", fixed=TRUE)
})

test_that("informative error when subject is named", {
  expect_error({
    nc::capture_all_str(chrom="chr.*?", ":", chromStart="[0-9,]+")
  }, "first argument is named chrom but must NOT be named; please include the subject to match as the first argument, with no name")
  expect_error({
    nc::capture_first_vec(chrom="chr.*?", ":", chromStart="[0-9,]+")
  }, "first argument is named chrom but must NOT be named; please include the subject to match as the first argument, with no name")
})
