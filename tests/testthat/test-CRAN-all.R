library(nc)
library(testthat)
library(data.table)
context("all")

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  test_engine("capture_all_str returns data.frame with 0 rows, 1 chr col", {
    subject <- c("foobar", "FOOBAR")
    computed <- capture_all_str(subject, baz="sars")
    expect_identical(computed$baz, character())
  })

  test_engine("capture_all_str returns data.frame with 0 rows, 1 int col", {
    subject <- c("foobar", "FOOBAR")
    computed <- capture_all_str(subject, baz="sars", as.integer)
    expect_identical(computed$baz, integer())
  })

  test_engine("capture_all_str returns data.table", {
    chr.pos.vec <- c(
      "chr10:213,054,000-213,055,000",
      "chrM:111,000-222,000",
      "this will not match",
      NA, # neither will this.
      "chr1:110-111 chr2:220-222") # two possible matches.
    keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
    computed <- capture_all_str(
      chr.pos.vec,
      chrom="chr.*?",
      ":",
      chromStart=".*?", keep.digits,
      "-",
      chromEnd="[0-9,]*", keep.digits)
    expected <- rbind(
      data.table(
        chrom="chr10", chromStart=213054000L, chromEnd=213055000L),
      data.table(
        chrom="chrM", chromStart=111000L, chromEnd=222000L),
      data.table(
        chrom=c("chr1", "chr2"),
        chromStart=as.integer(c("110", "220")),
        chromEnd=as.integer(c("111", "222"))))
    expect_identical(computed, expected)
  })

  test_engine("capture_all_str errors for one argument", {
    expect_error({
      capture_all_str("foo")
    }, "must have at least one named argument")
  })

  test_engine("capture_all_str errors for multi-dim patterns", {
    expect_error({
      capture_all_str("foo", sars=c("bar", "baz"))
    }, "patterns must be character vectors of length 1")
  })

  test_engine("capture_all_str errors for 0-length patterns", {
    expect_error({
      capture_all_str("foo", bar=character())
    }, "patterns must be character vectors of length 1")
  })

  test_engine("capture_all_str errors for non char/fun args", {
    expect_error({
      capture_all_str("foo", "bar", 1)
    }, "arguments must be", fixed=TRUE)
  })

  test_engine("capture_all_str errors for two funs in a row", {
    expect_error({
      capture_all_str("foo", g="bar", as.integer, as.numeric)
    },
    "too many functions; up to one function may follow each named pattern")
  })

  test_engine("capture_all_str errors for fun at start", {
    expect_error({
      capture_all_str("foo", as.numeric)
    },
    "too many functions; up to one function may follow each named pattern")
  })

  test_engine("capture_all_str errors for NA pattern", {
    expect_error({
      capture_all_str("foo", g="bar", NA_character_, "baz")
    }, "patterns must not be missing/NA")
  })

  trackDb.txt.gz <- system.file("extdata", "trackDb.txt.gz", package="nc")
  trackDb.vec <- readLines(trackDb.txt.gz)

  test_engine("nested capture groups works", {
    track.pattern <- list(
      cellType=".*?",
      "_",
      sampleName=list(as.factor,
                      "McGill",
                      sampleID="[0-9]+", as.integer),
      dataType="Coverage|Peaks",
      "|",
      "[^\n]+")
    match.df <- capture_all_str(
      trackDb.vec,
      "track ",
      track=track.pattern,
      "(?:\n[^\n]+)*",
      "\\s+bigDataUrl ",
      bigDataUrl="[^\n]+")
    expect_is(match.df, "data.frame")
    expect_identical(
      names(match.df),
      c("track", "cellType", "sampleName", "sampleID",
        "dataType", "bigDataUrl"))
    expect_is(match.df$sampleName, "factor")
    expect_is(match.df$sampleID, "integer")
  })

}
