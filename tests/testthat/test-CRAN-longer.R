library(nc)
library(testthat)
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_that("single spec works", {
  (single.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", dim=".*", values_to="cm"))
  expect_identical(sort(names(single.spec)), c(".name", ".value", "dim", "part"))
})

test_that("multiple spec works", {
  (multiple.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", column=".*"))
  expect_identical(sort(names(multiple.spec)), c(".name", ".value", "part"))
})
