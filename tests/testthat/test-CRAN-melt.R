library(nc)
library(testthat)
library(data.table)
context("melt")

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  data(who, package="tidyr", envir=environment())
  who.tidy <- nc::capture_first_melt(
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
