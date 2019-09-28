library(testthat)
library(nc)
context("multiple")
library(data.table)

iris.dt <- data.table(i=1:nrow(iris), iris)
iris.dt[, chr := paste(Species)]
set.seed(1)
iris.rand <- iris.dt[sample(.N)]
iris.wide <- cbind(treatment=iris.rand[1:75], control=iris.rand[76:150])
test_that("iris multiple column matches", {
  nc.melted <- capture_first_melt_multiple(
    iris.wide,
    group="[^.]+",
    "[.]",
    variable=".*")
  expect_identical(nc.melted$variable, factor(rep(1:2, each=75)))
  expect_identical(nc.melted$group, rep(c("treatment", "control"), each=75))
  nc.melted.orig <- nc.melted[order(i), names(iris.dt), with=FALSE]
  expect_identical(nc.melted.orig, iris.dt)
})

set.seed(1)
iris.wide.rand <- iris.wide[, sample(names(iris.wide)), with=FALSE]
test_that("iris multiple rand column matches", {
  rand.melted <- capture_first_melt_multiple(
    iris.wide.rand,
    group="[^.]+",
    "[.]",
    variable=".*"
  )[order(i), names(iris.dt), with=FALSE]
  expect_identical(rand.melted, iris.dt)
})

iris.wide.bad <- data.table(iris.wide.rand)
names(iris.wide.bad)[1] <- "treatment.Petal.bad"
test_that("iris multiple rand column matches", {
  expect_error({
    capture_first_melt_multiple(
      iris.wide.bad,
      group="[^.]+",
      "[.]",
      variable=".*")
  }, "need 2 values for each variable, problems: Petal.bad, Petal.Length")
})

## contrived example with some variables that do not have all columns
## -- we could support this but it may be better to stop with an error
## -- the user could create the NA columns if they really had to.
if(FALSE){
  iris.wide.odd <- cbind(iris.wide.rand, foo.Petal.Length=rnorm(nrow(iris.wide)))
  odd.melted <- capture_first_melt_multiple(
    iris.wide.odd,
    group="[^.]+",
    "[.]",
    variable=".*"
  )
}

DT <- data.table(
  i_1 = c(1:5, NA),
  i_2 = c(NA,6:10),
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01"))
## add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]
test_that("melt multiple column types error if no other group", {
  expect_error({
    capture_first_melt_multiple(DT, variable="^[^c]")
  }, "need at least one group other than variable")
})

test_that("melt multiple column types without id.vars", {
  result <- capture_first_melt_multiple(
    DT,
    variable="^[^c]",
    "_",
    number="[0-9]")
  expect_is(result$c_1, "character")
  expect_is(result$variable, "factor")
  expect_is(result$i, "integer")
  expect_is(result$f, "factor")
  expect_is(result$d, "Date")
  expect_is(result$l, "list")
  expect_identical(nrow(result), 12L)
})

test_that("melt multiple column types with id.vars", {
  id.result <- capture_first_melt_multiple(
    DT,
    variable="^[fl]",
    "_",
    number="[0-9]",
    id.vars=1:2)
  expect_is(id.result$variable, "factor")
  expect_is(id.result$i_1, "integer")
  expect_is(id.result$i_2, "integer")
  expect_is(id.result$f, "factor")
  expect_is(id.result$l, "list")
  expect_identical(nrow(id.result), 12L)
})

D2 <- fread(text="family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
test_that("gender dob example", {
  children <- capture_first_melt_multiple(
    D2,
    variable="[^_]+",
    between="_child",
    number="[1-3]",
    variable.name="child")
  expect_is(children$family_id, "integer")
  expect_is(children$age_mother, "integer")
  expect_is(children$child, "factor")
  expect_is(children$dob, "character")
  expect_is(children$gender, "integer")
})

test_that("multiple error if subject not df", {
  expect_error({
    capture_first_melt_multiple("foobar")
  }, "subject must be a data.frame")
})

test_that("multiple error if no arg named variable", {
  expect_error({
    capture_first_melt_multiple(D2, baz="foobar")
  }, "pattern must define group named variable")
})

test_that("multiple error if no matching column names", {
  expect_error({
    capture_first_melt_multiple(D2, variable="foobar")
  }, "no column names match regex")
})
