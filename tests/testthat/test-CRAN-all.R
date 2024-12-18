library(nc)
library(testthat)
library(data.table)
context("all")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_engines("capture_all_str has no warning about no such file", {
  expect_warning({
    capture_all_str("chr1:100", baz="sars")
  }, regexp=NA)
})

subject <- c("foobar", "FOOBAR")
test_engines("capture_all_str returns data.frame with 0 rows, 1 chr col", {
  computed <- capture_all_str(subject, baz="sars")
  expect_identical(computed$baz, character())
})

test_engines("capture_all_str returns data.frame with 0 rows, 1 int col", {
  subject <- c("foobar", "FOOBAR")
  computed <- capture_all_str(subject, baz="sars", as.integer)
  expect_identical(computed$baz, integer())
})

keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
test_engines("capture_all_str returns data.table", {
  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
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

test_engines("capture_all_str(type.convert=TRUE) returns one int column", {
  computed <- capture_all_str(
    "chr1:2-3,000 chr4:5-6,000",
    chrom="chr.*?",
    ":",
    chromStart=".*?",
    "-",
    chromEnd="[0-9,]*",
    type.convert=TRUE)
  expected <- data.table(
    chrom=c("chr1","chr4"),
    chromStart=c(2L,5L),
    chromEnd=c("3,000","6,000"))
  expect_identical(computed, expected)
})

test_engines("capture_all_str(type.convert=TRUE) returns two int columns", {
  computed <- capture_all_str(
    "chr1:2-3,000 chr4:5-6,000",
    chrom="chr.*?",
    ":",
    chromStart=".*?",
    "-",
    chromEnd="[0-9,]*", keep.digits,
    type.convert=TRUE)
  expected <- data.table(
    chrom=c("chr1","chr4"),
    chromStart=c(2L,5L),
    chromEnd=c(3000L,6000L))
  expect_identical(computed, expected)
})

test_engines("capture_all_str errors for one argument", {
  expect_error({
    capture_all_str("foo")
  }, "must have at least one named argument")
})

test_engines("capture_all_str errors for multi-dim patterns", {
  expect_error({
    capture_all_str("foo", sars=c("bar", "baz"))
  }, "patterns must be character vectors of length 1")
})

test_engines("capture_all_str errors for 0-length patterns", {
  expect_error({
    capture_all_str("foo", bar=character())
  }, "patterns must be character vectors of length 1")
})

test_engines("capture_all_str errors for non char/fun args", {
  expect_error({
    capture_all_str("foo", "bar", 1)
  }, "arguments must be", fixed=TRUE)
})

test_engines("capture_all_str errors for two funs in a row", {
  expect_error({
    capture_all_str("foo", g="bar", as.integer, as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_engines("capture_all_str errors for fun at start", {
  expect_error({
    capture_all_str("foo", as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_engines("capture_all_str errors for NA pattern", {
  expect_error({
    capture_all_str("foo", g="bar", NA_character_, "baz")
  }, "patterns must not be missing/NA")
})

test_engines("nested capture groups works", {
  trackDb.txt.gz <- system.file("extdata", "trackDb.txt.gz", package="nc")
  track.pattern <- list(
    cellType=".*?",
    "_",
    sampleName=list(as.factor,
                    "McGill",
                    sampleID="[0-9]+", as.integer),
    dataType="Coverage|Peaks",
    "|",
    "[^\n]+")
  match.dt <- capture_all_str(
    trackDb.txt.gz,
    "track ",
    track=track.pattern,
    "(?:\n[^\n]+)*",
    "\\s+bigDataUrl ",
    bigDataUrl="[^\n]+")
  expect_equal(nrow(match.dt), 78)
  expect_is(match.dt, "data.table")
  expect_identical(
    names(match.dt),
    c("track", "cellType", "sampleName", "sampleID",
      "dataType", "bigDataUrl"))
  expect_is(match.dt$sampleName, "factor")
  expect_is(match.dt$sampleID, "integer")
})

test_engines("error for capture all regex with literal groups, match", {
  expect_error({
    capture_all_str(
      c("chr1:100-200", "chr2:5-6"),
      chrom="chr.",
      ":",
      "([0-9]+)")
  }, "regex contains more groups than names; please remove literal groups (parentheses) from the regex pattern, and use named arguments in R code instead", fixed=TRUE)
})

test_engines("error for capture all regex with literal groups, no match", {
  expect_error({
    nc::capture_all_str("alias(es)", foo="alias(es)")
  }, "regex contains more groups than names; please remove literal groups (parentheses) from the regex pattern, and use named arguments in R code instead", fixed=TRUE)
})
