library(nc)
library(testthat)
library(data.table)
context("melt")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

set.seed(45)
DT <- data.table(
  i_1 = c(1:5, NA),
  i_2 = c(NA,6,7,8,9,10),
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01"))

iris.dt <- data.table(observation=1:nrow(iris), iris)
test_engines("error for regex that matches no column names", {
  expect_error({
    capture_melt_single(iris.dt, part="foo")
  }, "no column names match regex")
})

test_engines("possessive (.*+) error(RE2) or OK(others)", {
  posmatch <- function(){
    capture_melt_single(
      iris.dt,
      part=".*",
      "[.]",
      dim=".*+")
  }
  if(identical(getOption("nc.engine"), "RE2")){
    expect_error({
      posmatch()
    }, "bad repetition operator")
  }else{
    iris.tall <- posmatch()
    exp.names <- c(
      "observation", "Species", "part", "dim", "value")
    expect_identical(names(iris.tall), exp.names)
  }
})

test_engines("error if first arg not df", {
  expect_error({
    capture_melt_single("foo", bar="baz")
  }, "subject must be a data.frame")
})

## what if an input column is named .variable?
DV <- data.table(
  .variable=c("foo", "bar"), "p10.5"=c(3L, 5L), "p1.1"=c(0L, 1L))
test_engines(".variable input column ok", {
  tall.dt <- capture_melt_single(DV, "p", penalty=".*", as.numeric)
  expect_identical(tall.dt$penalty, c(10.5, 10.5, 1.1, 1.1))
})

## what if a capture group has the same name as variable.name?
test_engines("capture group with funny name is ok", {
  tall.dt <- capture_melt_single(
    DV, "p", .variable.p10.5.p1.1=".*", as.numeric)
  expect_identical(tall.dt$.variable.p10.5.p1.1, c(10.5, 10.5, 1.1, 1.1))
})

## what if input df has repeated names?
bad.df <- data.frame(1, 2)
names(bad.df) <- c("foo", "foo")
test_engines("melting df with same col names is an error", {
  expect_error({
    capture_melt_single(bad.df, o="o+")
  }, "input must have columns with unique names, problems: foo")
})

## what if there are two groups with the same name?
test_engines("groups with the same name is an error", {
  expect_error({
    capture_melt_single(DV, foo="p", foo="1")
  }, "capture group names must be unique, problem: foo")
})

## what if a capture group has the same name as an input column?
test_engines("err change capture group if same as input col", {
  expect_error({
    capture_melt_single(DV, .variable="p")
  },
  "some capture group names (.variable) are the same as input column names that did not match the pattern; please change either the pattern or the capture group names so that all output column names will be unique",
  fixed=TRUE)
})

## what if value.name is the same as input col?
test_engines("err change value.name if same as input col", {
  expect_error({
    capture_melt_single(DV, "p", num=".*", value.name=".variable")
  },
  "value.name (.variable) is the same as an input column name that did not match the pattern; please change one so that all output column names will be unique",
  fixed=TRUE)
})

test_engines("err change value.name or group names", {
  expect_error({
    capture_melt_single(DV, "p", num=".*", value.name="num")
  },
  "value.name (num) is the same as a capture group name; please change one so that all output column names will be unique",
  fixed=TRUE)
})

i.vec <- 1:10000
one.row <- data.frame(t(i.vec))
test_engines("melting lots of columns is OK", {
  out <- capture_melt_single(one.row, "X", col="[0-9]+", as.integer)
  expect_identical(out$col, i.vec)
  expect_identical(out$value, i.vec)
})
