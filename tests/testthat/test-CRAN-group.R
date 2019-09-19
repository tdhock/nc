library(testthat)
library(nc)
context("group")

test_that("group returns named list", {
  computed <- group("sample", "McGill", id="[0-9]+", as.integer)
  pat <- list("McGill", id="[0-9]+", as.integer)
  expected <- structure(list(pat), names="sample")
  expect_identical(computed, expected)
})
