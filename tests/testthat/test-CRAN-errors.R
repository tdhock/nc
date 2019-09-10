library(nc)
library(testthat)
context("errors")

test_that("error for unknown engine", {
  expect_error({
    capture_first_vec("foo", bar="foo", engine="sars")
  }, "engine must be character string")
})

expect_error_engine_pkg <- function(engine, pkg){
  if(isNamespaceLoaded(pkg))unloadNamespace(pkg)
  pkg.dir <- system.file(package=pkg)
  new.dir <- paste0(pkg.dir, "_tmp")
  file.rename(pkg.dir, new.dir)
  expect_error({
    capture_first_vec("foo", bar="foo", engine=engine)
  }, pkg)
  file.rename(new.dir, pkg.dir)
}
test_that("error tells user to install re2r when RE2 not available", {
  expect_error_engine_pkg("RE2", "re2r")
})
test_that("error tells user to install stringi when ICU not available", {
  expect_error_engine_pkg("ICU", "stringi")
})

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  foo.bar <- c("foo", "bar")
  test_engine("no capture groups is an error", {
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
  test_engine("named character vector is an error with group name", {
    expect_error_with_code({
      capture_first_vec(foo.bar, gname=named.chr.vec)
    }, expected.obj)
  })
  test_engine("named character vector is an error without group name", {
    expect_error_with_code({
      capture_first_vec(foo.bar, named.chr.vec)
    }, expected.obj)
  })

  named.pat.list <- as.list(named.chr.vec)
  exp.vec <- c("oo", "a")
  test_engine("named pattern list in named arg makes two groups", {
    (result.df <- capture_first_vec(foo.bar, gname=named.pat.list))
    expect_identical(names(result.df), c("gname", names(named.pat.list)))
    expect_identical(result.df$gname, exp.vec)
    expect_identical(result.df[[names(named.pat.list)]], exp.vec)
  })
  test_engine("named pattern list in un-named arg makes one group", {
    (result.df <- capture_first_vec(foo.bar, named.pat.list))
    expect_identical(names(result.df), names(named.pat.list))
    expect_identical(result.df[[names(named.pat.list)]], exp.vec)
  })

  test_engine("any capture group without a name is an error", {
    expect_error({
      capture_first_vec(foo.bar, "(o)(?P<name>o)")
    }, "must have at least one named argument", fixed=TRUE)
  })

  test_engine("NA pattern is an error", {
    expect_error({
      capture_first_vec(foo.bar, NA_character_)
    }, "patterns must not be missing/NA", fixed=TRUE)
  })

  test_engine("factor pattern is an error", {
    expect_error({
      capture_first_vec(foo.bar, factor("(?P<regex>foo)"))
    }, "arguments must be character")
  })

  test_engine("multiple patterns is an error", {
    expect_error({
      capture_first_vec(foo.bar, c("(?P<name>.)", "(?P<name>.)"))
    }, "patterns must be character vectors of length 1")
  })

  test_engine("subject of length 0 is an error", {
    expect_error({
      capture_first_vec(character(), "(?P<name>.)")
    }, "subject.vec should be a character vector with length>0", fixed=TRUE)
  })

  test_engine("capture all works with only one 'name' group", {
    subject <- c(missing=NA, nomatch="", match="foobar")
    result.df <- capture_all_str(subject, name="foo")
    expect_null(names(result.df))
    expect_identical(rownames(result.df), "foo")
  })

  test_engine("informative error when converter fun has zero args", {
    expect_error({
      capture_first_vec(
        "chr2:300-400",
        chrom="chr", function()y)
    }, "type.list must be list(group.name=function(character.vector)atomic.vector)",
    fixed=TRUE)
  })

  test_engine("informative error when converter returns wrong length", {
    expect_error({
      capture_first_vec(
        c("chr2:300-400", "chr2:300-400"),
        chrom="chr", function(x)"foo")
    }, "type conversion function for group chrom returned vector of length 1 but expected length 2")
  })

  test_engine("informative error when converter returns non-atomic", {
    expect_error({
      capture_first_vec(
        c("chr2:300-400", "chr2:300-400"),
        chrom="chr", function(x)list(foo=200))
    }, "type conversion function for group chrom must return atomic vector")
  })

  test_engine("informative error for non-unique chromStart int names", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", "chr2:300-400", "chr2:300-400"),
        chrom="[^:]+",
        ":",
        name="[0-9]+", as.integer)
    }, "capture group named 'name' must be unique")
  })

  test_engine("informative error for non-unique chromStart names", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", "chr2:300-400", "chr2:300-400"),
        chrom="[^:]+",
        ":",
        name="[0-9]+")
    }, "capture group named 'name' must be unique")
  })

  test_engine("informative error for non-unique chrom names", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", "chr2:300-400", "chr2:300-400"),
        name="[^:]+",
        ":",
        chromStart="[0-9]+")
    }, "capture group named 'name' must be unique")
  })

  test_engine("error for name group, missing subject, nomatch.error=FALSE", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", NA, "chr2:300-400"),
        name="[^:]+",
        ":",
        chromStart="[0-9]+",
        nomatch.error=FALSE)
    }, "must use nomatch.error=TRUE with name group")
  })

  test_engine("error for name group, no match, nomatch.error=FALSE", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", "foobar", "chr2:300-400"),
        name="[^:]+",
        ":",
        chromStart="[0-9]+",
        nomatch.error=FALSE)
    }, "must use nomatch.error=TRUE with name group")
  })

  test_engine("error for name group, missing subject, nomatch.error=TRUE", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", NA, "chr2:300-400"),
        name="[^:]+",
        ":",
        chromStart="[0-9]+",
        nomatch.error=TRUE)
    }, "subjects printed above did not match regex below")
  })

  test_engine("error for name group, no match, nomatch.error=TRUE", {
    expect_error({
      capture_first_vec(
        c("chr1:20-40", "foobar", "chr2:300-400"),
        name="[^:]+",
        ":",
        chromStart="[0-9]+",
        nomatch.error=TRUE)
    }, "subjects printed above did not match regex below")
  })

  name.value.vec <- c(
    "  sampleType=monocyte   assayType=H3K27me3    cost=5",
    "sampleType=monocyte assayType=H3K27ac",
    " assayType=Myeloidcell cost=30.5  assayType=H3K4me3")
  name.value.pattern <- list(
    name="[^ ]+?",
    "=",
    value="[^ ]+")
  test_engine("error for non-unique name in match_all", {
    expect_error({
      capture_all_str(name.value.vec, name.value.pattern)
    }, "capture group named 'name' must be unique")
  })

}
