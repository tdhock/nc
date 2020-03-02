library(nc)
library(testthat)
context("alternatives")

test_that("error for no arguments to alternatives", {
  expect_error({
    alternatives()
  }, "alternatives should have at least two arguments")
})

test_that("error for one argument to alternatives", {
  expect_error({
    alternatives("pattern")
  }, "alternatives should have at least two arguments")
})

test_that("two un-named alternatives => two non-capturing groups", {
  subject <- c("foo1", "bar2")
  a.pat <- alternatives("foo", "bar")
  match.dt <- nc::capture_first_vec(subject, a.pat, number="[12]")
  expect_identical(match.dt, data.table(number=paste(1:2)))
})

subject <- c("foooo1", "bar2")
exp.dt <- data.table(foo=c("foooo", ""), number=paste(1:2))
test_that("first named alternative => one more capturing group", {
  a.pat <- alternatives(foo="fo+", "bar")
  match.dt <- nc::capture_first_vec(subject, a.pat, number="[12]")
  expect_identical(match.dt, exp.dt)
})

test_that("second named alternative => one more capturing group", {
  a.pat <- alternatives("bar", foo="fo+")
  match.dt <- nc::capture_first_vec(subject, a.pat, number="[12]")
  expect_identical(match.dt, exp.dt)
})

test_that("both named alternatives => two more capturing groups", {
  subject <- c("foooo1", "barrr2")
  exp.dt <- data.table(
    bar=c("", "barrr"),
    foo=c("foooo", ""),
    number=paste(1:2))
  a.pat <- alternatives(bar="bar+", foo="fo+")
  match.dt <- nc::capture_first_vec(subject, a.pat, number="[12]")
  expect_identical(match.dt, exp.dt)
})

