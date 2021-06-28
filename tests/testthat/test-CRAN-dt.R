library(testthat)
library(nc)
library(data.table)
context("dt")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_engines("default error for existing column, otherwise modify", {
  pos <- "chr1:100-200"
  pos.dt <- data.table(pos)
  int.pattern <- list("[0-9]+", as.integer)
  chrom.start.pattern <- list(
    chrom=".*?",
    ":",
    start=int.pattern)
  nc::capture_first_df(pos.dt, pos=chrom.start.pattern)
  expect_identical(pos.dt, data.table(pos, chrom="chr1", start=100L))
  full.pattern <- list(chrom.start.pattern, "-", end=int.pattern)
  expect_error({
    nc::capture_first_df(pos.dt, pos=full.pattern)
  }, "capture group names (chrom, start, end) must not conflict with existing column names (pos, chrom, start); fix by changing capture group names or use existing.error=FALSE to overwrite existing column names", fixed=TRUE)
  nc::capture_first_df(pos.dt, pos=full.pattern, existing.error=FALSE)
  expect_identical(pos.dt, data.table(pos, chrom="chr1", start=100L, end=200L))
})
