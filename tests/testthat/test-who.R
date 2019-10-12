library(testthat)
library(nc)
context('who')

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  data(who, package="tidyr", envir=environment())
  who.tall <- capture_first_melt(
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
    expect_is(who.tall, "data.table")
    exp.names <- c(
      "diagnosis", "gender", "ages",
      "min.years", "max.years", "count", "column")
    expect_true(all(exp.names %in% names(who.tall)))
  })

  test_engine("melt_multiple errors with one output column", {
    expect_error({
      capture_first_melt_multiple(who, variable=list(
        column="new",
        maybe="_?",
        diagnosis=".*",
        value="_",
        gender=".",
        ages=list(
          min.years="0|[0-9]{2}", as.numeric,
          max.years=list("[0-9]{2}"), "?",
          function(x)ifelse(x=="", Inf, as.numeric(x)))))
    }, "need multiple output columns, but only one value (new) captured in column group; either provide a different regex that captures more than one value in column group, or use capture_first_melt if you really want only one output column", fixed=TRUE)
  })

}
