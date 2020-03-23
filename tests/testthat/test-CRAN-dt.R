library(testthat)
library(nc)
library(data.table)
context("dt")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

subject.dt <- data.table(
  JobID=c(
    "13937810_25",
    "13937810_25.batch",
    "13937810_25.extern",
    "14022192_[1-3]",
    "14022204_[4]"),
  position=c(
    "chr10:213,054,000-213,055,000",
    "chrNA:111,000-222,000",
    "foo bar",
    NA,
    "chr1:110-111 chr2:220-222"),
  stringsAsFactors=FALSE)
range.pattern <- list(
  "\\[",
  task1="[0-9]+", as.integer,
  list(
    "-",#begin optional end of range.
    taskN="[0-9]+", as.integer
  ), "?", #end is optional.
  "\\]")
test_engines("capture_first_df returns data.table", {
  match.dt <- capture_first_df(
    subject.dt,
    JobID=list(
      job="[0-9]+", as.integer,
      "_",
      "(?:",#begin alternate
      task="[0-9]+", as.integer,
      "|",#either one task(above) or range(below)
      range.pattern,
      ")",#end alternate
      "(?:[.]",
      type=".*",
      ")?"),
    position=list(
      nomatch.error=FALSE,
      chrom="chr.*?",
      ":",
      chromStart=".*?",
      "-",
      chromEnd="[0-9,]*"))
  expect_identical(names(match.dt), c(
    "JobID", "position",
    "job", "task", "task1", "taskN", "type",
    "chrom", "chromStart", "chromEnd"))
  expect_identical(match.dt$job, as.integer(c(
    13937810, 13937810, 13937810, 14022192, 14022204)))
  expect_identical(match.dt$task, as.integer(c(
    25, 25, 25, NA, NA)))
  expect_identical(match.dt$task1, as.integer(c(
    NA, NA, NA, 1, 4)))
  expect_identical(match.dt$taskN, as.integer(c(
    NA, NA, NA, 3, NA)))
  expect_identical(match.dt$type, c(
    "", "batch", "extern", "", ""))
  expect_identical(match.dt$chrom, c(
    "chr10", "chrNA", NA, NA, "chr1"))
  expect_identical(match.dt$chromStart, c(
    "213,054,000", "111,000", NA, NA, "110"))
  expect_identical(match.dt$chromEnd, c(
    "213,055,000", "222,000", NA, NA, "111"))
  expect_is(match.dt, "data.table")
})
