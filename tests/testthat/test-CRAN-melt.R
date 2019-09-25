library(nc)
library(testthat)
library(data.table)
context("melt")

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

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
