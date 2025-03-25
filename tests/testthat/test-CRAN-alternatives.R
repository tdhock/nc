library(nc)
library(data.table)
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

pat.list <- nc::altlist(month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}")
pattern <- with(pat.list, nc::alternatives(
  list(month, " ", day, ", ", year),
  list(day, " ", month, " ", year)))
test_that("alternatives with dup names ok", {
  subject.vec <- c("mar 17, 1983", "26 sep 2017")
  computed <- nc::capture_first_vec(subject.vec, pattern)
  expected <- data.table(
    month=c("mar", "sep"), day=c("17", "26"), year=c("1983", "2017"))
  expect_identical(computed, expected)
})
test_that("alternatives with dup names and one subject with no match", {
  subject.vec <- c("mar 17, 1983", "this will not match", "26 sep 2017")
  computed <- nc::capture_first_vec(subject.vec, pattern, nomatch.error=FALSE)
  expected <- data.table(
    month=c("mar", NA, "sep"),
    day=c("17", NA, "26"),
    year=c("1983", NA, "2017"))
  expect_identical(computed, expected)
})

test_that("alternatives with dup names error for diff types", {
  subject.vec <- c("mar 17, 1983", "26 sep 2017")
  pat.list <- nc::altlist(month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}")
  pattern <- with(pat.list, nc::alternatives(
    list(month, " ", day, ", ", year, as.integer),
    list(day, " ", month, " ", year)))
  expect_error({
    nc::capture_first_vec(subject.vec, pattern)
  }, "capture groups with identical names should have conversion functions that all return the same type; problem group name=year has types character,integer")
})

test_that("IDate conversion OK", {
  subject.vec <- c(
    "02/07/2020 EUR68.50",
    "EUR45.00 29/10/2020")
  pat.list <- nc::altlist(
    money=list("EUR", amount="[0-9.]+", as.numeric),
    date=list(
      "[0-9]{2}/[0-9]{2}/[0-9]{4}",
      function(d)data.table::as.IDate(d, format="%d/%m/%Y")))
  pattern <- with(pat.list, nc::alternatives(
    list(money, " ", date),
    list(date, " ", money)))
  computed <- nc::capture_first_vec(subject.vec, pattern)
  expected <- data.table(
    money=c("EUR68.50", "EUR45.00"),
    amount=c(68.5, 45),
    date=as.IDate(c("2020-07-02", "2020-10-29")))
  expect_identical(computed, expected)
})

test_that("altlist fails for no names", {
  expect_error({
    nc::altlist("foo")
  }, "all arguments to altlist must be named")
})

test_that("altlist fails for no args", {
  expect_error({
    nc::altlist()
  }, "all arguments to altlist must be named")
})

test_that("altlist fails for one un-named arg", {
  expect_error({
    nc::altlist(foo="bar", "baz")
  }, "all arguments to altlist must be named")
})

test_that("alternatives_with_shared_groups errors for no named args", {
  expect_error({
    nc::alternatives_with_shared_groups("bar")
  }, "alternatives_with_shared_groups must have at least one named argument; each named argument should define a sub-pattern/group")
})

test_that("alternatives_with_shared_groups errors for no un-named args", {
  expect_error({
    nc::alternatives_with_shared_groups(foo="bar")
  }, "alternatives_with_shared_groups must have at least one un-named argument; each un-named argument should define an alternative pattern")
})

test_that("altlist + alternatives with names ok", {
  subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
  pat.list <- nc::altlist(month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}")
  pattern <- with(pat.list, nc::alternatives(
    american=list(month, " ", day, ", ", year),
    european=list(day, " ", month, " ", year)))
  match.dt <- nc::capture_first_vec(subject.vec, pattern)
  sorted.names <- c("american", "day", "european", "month", "year")
  expect_identical(sort(names(match.dt)), sorted.names)
  expect_identical(match.dt[["american"]], c(subject.vec[1], "", ""))
  expect_identical(match.dt[["european"]], c("", subject.vec[2:3]))
  expect_identical(match.dt[["month"]], c("mar", "sep", "mar"))
  expect_identical(match.dt[["day"]], c("17", "26", "17"))
  expect_identical(match.dt[["year"]], c("1983", "2017", "1984"))
})

test_that("alternatives_with_shared_groups ok with 3 subjects", {
  subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
  pattern <- nc::alternatives_with_shared_groups(
    month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}",
    list(month, " ", day, ", ", year),
    list(day, " ", month, " ", year))
  match.dt <- nc::capture_first_vec(subject.vec, pattern)
  sorted.names <- c("day", "month", "year")
  expect_identical(sort(names(match.dt)), sorted.names)
  expect_identical(match.dt[["month"]], c("mar", "sep", "mar"))
  expect_identical(match.dt[["day"]], c("17", "26", "17"))
  expect_identical(match.dt[["year"]], c("1983", "2017", "1984"))
})

test_that("alternatives_with_shared_groups ok with 1 subject", {
  subject.vec <- "mar 17, 1983"
  pattern <- nc::alternatives_with_shared_groups(
    month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}",
    list(month, " ", day, ", ", year),
    list(day, " ", month, " ", year))
  match.dt <- nc::capture_first_vec(subject.vec, pattern)
  sorted.names <- c("day", "month", "year")
  expect_identical(sort(names(match.dt)), sorted.names)
  expect_identical(match.dt[["month"]], "mar")
  expect_identical(match.dt[["day"]], "17")
  expect_identical(match.dt[["year"]], "1983")
})

test_that("alevels ok with no names", {
  ifac <- nc::capture_melt_single(
    iris[1,],
    part=nc::alevels("Sepal","Petal"),
    "[.]",
    dim=nc::alevels("Length","Width"))
  expect_identical(levels(ifac$part), c("Sepal","Petal"))
  expect_identical(levels(ifac$dim), c("Length","Width"))
})

test_that("alevels() ok with all and some names", {
  tv_wide <- data.frame(
    id=0,
    train.classif.logloss = 1, train.classif.ce = 2,
    valid.classif.logloss = 3, valid.classif.ce = 4)
  tv_long <- nc::capture_melt_single(
    tv_wide,
    set_chr=list(set_fac=nc::alevels(valid="validation", train="subtrain")),
    "[.]classif[.]",
    measure_chr=list(measure_fac=nc::alevels(ce="error_prop", auc="AUC", "logloss")))
  expect_is(tv_long$set_chr, "character")
  expect_is(tv_long$measure_chr, "character")
  expect_identical(levels(tv_long$set_fac), c("validation","subtrain"))
  expect_identical(levels(tv_long$measure_fac), c("error_prop","AUC","logloss"))
})
