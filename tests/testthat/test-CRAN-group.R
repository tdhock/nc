library(testthat)
library(nc)
context("group")

test_that("group returns named list", {
  computed <- group("foo", ".*")
  expected <- structure(list(".*"), names="foo")
  expect_identical(computed, expected)
})
