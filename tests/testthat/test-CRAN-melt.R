library(nc)
library(testthat)
library(data.table)
context("melt")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

iris.dt <- data.table(observation=1:nrow(iris), iris)
test_engines("error for regex that matches no column names", {
  expect_error({
    capture_melt_single(iris.dt, part="foo")
  }, "no column names match regex")
})

test_engines("error for fun always returning NA", {
  expect_error(suppressWarnings({
    capture_melt_single(iris.dt, part=".*", as.numeric)
  }),
  "need to change type conversion function(s), which should return at least one non-NA, but are always returning NA, even though regex matched 6 column(s): observation,Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species",
  fixed=TRUE)
})

test_engines("error for fun always returning NA abbrev cols", {
  subject <- data.frame(t(1:99))
  expect_error(suppressWarnings({
    capture_melt_single(subject, part=".*", as.numeric)
  }),
  "need to change type conversion function(s), which should return at least one non-NA, but are always returning NA, even though regex matched 99 column(s): X1,X2,X3,X4,X5,...,X95,X96,X97,X98,X99",
  fixed=TRUE)
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
  }, "first argument (subject) must be a data.frame", fixed=TRUE)
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
  }, "duplicate capture group names are only allowed in alternatives, problem: foo")
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

DT.wide <- data.table(id=0, num_ref=1, name_ref="foo", num=2, name="bar")
test_engines("converting NA to non-NA is an error", {
  expect_error({
    nc::capture_melt_multiple(DT.wide, column="name|num", type=".*", function(x)fcase(x=="", "other", default="reference"))
  }, "a non-match(NA) was converted to a match(non-NA) by the conversion function in group 2(type); please fix conversion function", fixed=TRUE)
})
