library(nc)
library(data.table)
library(testthat)
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_engines("single spec works", {
  (single.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", dim=".*", values_to="cm"))
  expect_identical(sort(names(single.spec)), c(".name", ".value", "dim", "part"))
})

test_engines("multiple spec works", {
  (multiple.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", column=".*"))
  expect_identical(sort(names(multiple.spec)), c(".name", ".value", "part"))
})

if(requireNamespace("tidyr"))test_engines("missing spec works", {
  missing.col <- data.table(Sepal.Length=1, Sepal.Width=2, Petal.Length=3)
  (missing.spec <- nc::capture_longer_spec(missing.col, part=".*", "[.]", column=".*"))
  computed.df <- tidyr::pivot_longer_spec(missing.col, missing.spec)
  sorted.dt <- data.table(computed.df)[order(part)]
  expected.dt <- data.table(part=c("Petal", "Sepal"), Length=c(3, 1), Width=c(NA, 2))
  expect_identical(sorted.dt, expected.dt)
})

