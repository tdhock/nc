library(testthat)
library(nc)
context("group")

test_that("group returns named list", {
  computed <- group("sample", "McGill", id="[0-9]+", as.integer)
  pat <- list("McGill", id="[0-9]+", as.integer)
  expected <- structure(list(pat), names="sample")
  expect_identical(computed, expected)
})

test_that("group errors for non-character name", {
  expect_error({
    group(factor('foo'), "bar", 'BAZ')
  },
  "first argument of group must be a character string (group name)",
  fixed=TRUE)
})

test_that("group errors for NA character name", {
  l <- group(NA_character_, "bar", 'BAZ')
  ## missing names could also be manually constructed so no need to
  ## stop with an error in group, but we should stop with an error
  ## when the NA is parsed.
  expect_error({
    var_args_list(l)
  }, "group name must not be missing", fixed=TRUE)
})
