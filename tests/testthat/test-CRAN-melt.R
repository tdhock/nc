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

  exp.number.vec <- rep(c("1", "2"), each=nrow(DT))
  exp.name.vec <- c("f_1", "f_2", "number", "value")
  test_that("capture_melt_single passes id.vars to melt.data.table", {
    result <- capture_melt_single(
      DT,
      "d_",
      number="[12]",
      id.vars=c("f_1", "f_2"))
    expect_identical(names(result), exp.name.vec)
    expect_identical(result$number, exp.number.vec)
    expect_equal(sum(is.na(result$value)), 1)
  })

  test_that("capture_melt_single passes na.rm to melt.data.table", {
    result.rm <- capture_melt_single(
      DT,
      "d_",
      number="[12]",
      id.vars=c("f_1", "f_2"),
      na.rm=TRUE)
    expect_identical(names(result.rm), exp.name.vec)
    expect_equal(sum(is.na(result.rm$value)), 0)
  })

  iris.dt <- data.table(observation=1:nrow(iris), iris)
  test_engine("error for regex that matches no column names", {
    expect_error({
      capture_melt_single(iris.dt, part="foo")
    }, "no column names match regex")
  })

  test_engine("possessive (.*+) error(RE2) or OK(others)", {
    posmatch <- function(){
      capture_melt_single(
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
        "observation", "Species", "part", "dim", "value")
      expect_identical(names(iris.tall), exp.names)
    }
  })

  test_engine("error if first arg not df", {
    expect_error({
      capture_melt_single("foo", bar="baz")
    }, "subject must be a data.frame")
  })

  ## what if an input column is named .variable?
  DV <- data.table(
    .variable=c("foo", "bar"), "p10.5"=c(3L, 5L), "p1.1"=c(0L, 1L))
  test_engine(".variable input column ok", {
    tall.dt <- capture_melt_single(DV, "p", penalty=".*", as.numeric)
    expect_identical(tall.dt$penalty, c(10.5, 10.5, 1.1, 1.1))
  })

  ## what if a capture group has the same name as variable.name?
  test_that("capture group named input columns ok", {
    tall.dt <- capture_melt_single(
      DV, "p", .variable.p10.5.p1.1=".*", as.numeric)
    expect_identical(tall.dt$.variable.p10.5.p1.1, c(10.5, 10.5, 1.1, 1.1))
  })

}
