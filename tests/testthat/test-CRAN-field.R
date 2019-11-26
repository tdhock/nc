library(testthat)
library(nc)
context("field")

test_that("field returns list", {
  int.pattern <- list("[0-9]+", as.integer)
  cell.sample.type <- list(
    cellType="[^ ]*?",
    "_",
    sampleName=list(
      "McGill",
      sampleID=int.pattern),
    dataType="Coverage|Peaks")
  computed <- nc::field(
    "track",
    " ",
    cell.sample.type,
    "|",
    "[^\n]+")
  expected <- list(
    "track",
    " ",
    list(track=list(
      cell.sample.type,
      "|",
      "[^\n]+")))
  expect_identical(computed, expected)
})

test_that("field errors for non-character first arg", {
  expect_error({
    field(identity, "bar", "sars")
  },
  "first argument of field must be character string (field name)",
  fixed=TRUE)
})
