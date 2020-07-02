library(testthat)
library(data.table)
context('subject')

test_that("capture_first_vec does not use s arg for subject", {
  dt <- nc::capture_first_vec(
    "author: toby, subject: ml",
    s="\\S+$")
  expect_identical(dt, data.table(s="ml"))
})

test_that("capture_all_str does not use s arg for subject", {
  dt <- nc::capture_all_str(
    "author: toby, subject: ml",
    s="\\S+$")
  expect_identical(dt, data.table(s="ml"))
})

test_that("capture_first_df does not use s arg for subject", {
  subject <- data.frame(x="author: toby, subject: ml")
  dt <- nc::capture_first_df(subject, x=list(s="\\S+$"))
  expected.dt <- data.table(subject, s="ml")
  expect_identical(dt, expected.dt)
})

test_that("capture_melt_single does not use s arg for subject", {
  subject <- data.frame(family=1, s2=2, s3=3)
  ## also make sure that engine is passed thru (not interpreted as a
  ## capture group).
  dt <- nc::capture_melt_single(subject, s="[0-9]", engine="ICU")
  expected.dt <- data.table(
    family=1,
    s=c("2", "3"),
    value=c(2, 3))
  expect_identical(dt, expected.dt)
})

test_that("capture_melt_multiple does not use s arg for subject", {
  subject <- data.frame(family=1, d2=2, d3=3, c2="a", c3="b")
  ## also make sure that engine is passed thru (not interpreted as a
  ## capture group).
  dt <- nc::capture_melt_multiple(
    subject, column="[dc]", s="[0-9]", engine="ICU")
  expect_identical(dt[["family"]], c(1, 1))
  expect_identical(dt[["d"]], c(2, 3))
  expect_identical(dt[["c"]], c("a", "b"))
  expect_identical(dt[["s"]], c("2", "3"))
})

