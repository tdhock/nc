library(nc)
library(testthat)
library(data.table)
context("melt")

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  set.seed(45)
  DT <- data.table(
    i_1 = c(1:5, NA),
    i_2 = c(NA,6,7,8,9,10),
    f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
    f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
    c_1 = sample(c(letters[1:3], NA), 6, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
  ## add a couple of list cols
  DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
  DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]

  test_that("capture_first_melt passes id.vars to melt.data.table", {
    result <- capture_first_melt(
      DT,
      "d_",
      number="[12]",
      id.vars=c("f_1", "f_2"))
    exp.name.vec <- c("f_1", "f_2", "variable", "value", "number")
    expect_identical(names(result), exp.name.vec)
  })

  iris.dt <- data.table(observation=1:nrow(iris), iris)
  test_engine("error for regex that matches no column names", {
    expect_error({
      capture_first_melt(iris.dt, part="foo")
    }, "no column names match regex")
  })

  test_engine("possessive (.*+) error(RE2) or OK(others)", {
    posmatch <- function(){
      capture_first_melt(
        iris.dt,
        part=".*",
        "[.]",
        dim=".*+",
        engine=engine)
    }
    if(engine=="RE2"){
      expect_error({
        posmatch()
      }, "bad repetition operator")
    }else{
      iris.tall <- posmatch()
      exp.names <- c(
        "observation", "Species", "variable", "value", "part", "dim")
      expect_true(all(exp.names %in% names(iris.tall)))
    }
  })

  data(who, package="tidyr", envir=environment())
  who.tidy <- capture_first_melt(
    who,
    "new_?",
    diagnosis=".*",
    "_",
    gender=".",
    ages=list(
      min.years="0|[0-9]{2}", as.numeric,
      max.years=list("[0-9]{2}"), "?",
      function(x)ifelse(x=="", Inf, as.numeric(x))),
    value.name="count",
    variable.name="column")
  test_engine("capture_first_melt returns data.table", {
    expect_is(who.tidy, "data.table")
    exp.names <- c(
      "diagnosis", "gender", "ages",
      "min.years", "max.years", "count", "column")
    expect_true(all(exp.names %in% names(who.tidy)))
  })

  test_engine("error if first arg not df", {
    expect_error({
      capture_first_melt("foo", bar="baz")
    }, "subject must be a data.frame")
  })

}
