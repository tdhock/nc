library(nc)
library(testthat)
context("all")

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  test_engine("str_capture_all returns data.frame", {
    chr.pos.vec <- c(
      "chr10:213,054,000-213,055,000",
      "chrM:111,000-222,000",
      "this will not match",
      NA, # neither will this.
      "chr1:110-111 chr2:220-222") # two possible matches.
    keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
    computed <- str_capture_all(
      chr.pos.vec,
      chrom="chr.*?",
      ":",
      chromStart=".*?", keep.digits,
      "-",
      chromEnd="[0-9,]*", keep.digits)
    expected <- rbind(
      data.frame(
        chrom="chr10", chromStart=213054000L, chromEnd=213055000L,
        stringsAsFactors=FALSE),
      data.frame(
        chrom="chrM", chromStart=111000L, chromEnd=222000L,
        stringsAsFactors=FALSE),
      data.frame(
        chrom=c("chr1", "chr2"),
        chromStart=as.integer(c("110", "220")),
        chromEnd=as.integer(c("111", "222")),
        stringsAsFactors=FALSE))
    expect_identical(computed, expected)
  })

  test_engine("str_capture_all errors for one argument", {
    expect_error({
      str_capture_all("foo")
    }, "must have at least one named argument")
  })

  test_engine("str_capture_all errors for multi-dim patterns", {
    expect_error({
      str_capture_all("foo", sars=c("bar", "baz"))
    }, "patterns must be character vectors of length 1")
  })

  test_engine("str_capture_all errors for 0-length patterns", {
    expect_error({
      str_capture_all("foo", bar=character())
    }, "patterns must be character vectors of length 1")
  })

  test_engine("str_capture_all errors for non char/fun args", {
    expect_error({
      str_capture_all("foo", "bar", 1)
    }, "arguments must be", fixed=TRUE)
  })

  test_engine("str_capture_all errors for two funs in a row", {
    expect_error({
      str_capture_all("foo", g="bar", as.integer, as.numeric)
    },
    "too many functions; up to one function may follow each named pattern")
  })

  test_engine("str_capture_all errors for fun at start", {
    expect_error({
      str_capture_all("foo", as.numeric)
    },
    "too many functions; up to one function may follow each named pattern")
  })

  test_engine("str_capture_all errors for NA pattern", {
    expect_error({
      str_capture_all("foo", g="bar", NA_character_, "baz")
    }, "patterns must not be missing/NA")
  })

  trackDb.txt.gz <- system.file("extdata", "trackDb.txt.gz", package="nc")
  trackDb.vec <- readLines(trackDb.txt.gz)

  test_engine("nested capture groups works", {
    name.pattern <- list(
      cellType=".*?",
      "_",
      sampleName=list(as.factor,
                      "McGill",
                      sampleID="[0-9]+", as.integer),
      dataType="Coverage|Peaks",
      "|",
      "[^\n]+")
    match.df <- str_capture_all(
      trackDb.vec,
      "track ",
      name=name.pattern,
      "(?:\n[^\n]+)*",
      "\\s+bigDataUrl ",
      bigDataUrl="[^\n]+")
    expect_is(match.df, "data.frame")
    expect_identical(
      names(match.df),
      c("cellType", "sampleName", "sampleID", "dataType", "bigDataUrl"))
    expect_is(match.df$sampleName, "factor")
    expect_is(match.df$sampleID, "integer")
  })

}
