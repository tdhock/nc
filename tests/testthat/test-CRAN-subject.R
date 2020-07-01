library(testthat)
context('subject')

test_that("capture_first_vec does not use s arg for subject", {
  dt <- nc::capture_first_vec(
    "author: toby, subject: ml",
    s="\\S+$")
  expect_identical(dt[["s"]], "ml")
})

test_that("capture_all_str does not use s arg for subject", {
  dt <- nc::capture_all_str(
    "author: toby, subject: ml",
    s="\\S+$")
  expect_identical(dt[["s"]], "ml")
})

test_that("capture_first_df does not use s arg for subject", {
  dt <- nc::capture_first_df(
    data.frame(x="author: toby, subject: ml"),
    x=list(s="\\S+$"))
  expect_identical(dt[["s"]], "ml")
})

