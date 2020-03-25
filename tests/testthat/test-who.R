library(testthat)
library(nc)
context('who')
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

data(who, package="tidyr", envir=environment())
who.tall <- capture_melt_single(
  who,
  "new_?",
  diagnosis=".*",
  "_",
  gender=".",
  ages=list(
    min.years="0|[0-9]{2}", as.numeric,
    max.years=list("[0-9]{2}"), "?",
    function(x)ifelse(x=="", Inf, as.numeric(x))),
  value.name="count")
test_engines("capture_melt_single returns data.table", {
  expect_is(who.tall, "data.table")
  exp.names <- c(
    "country", "iso2", "iso3", "year",
    "diagnosis", "gender", "ages",
    "min.years", "max.years", "count")
  expect_identical(names(who.tall), exp.names)
})

test_engines("melt_multiple errors with one output column", {
  expect_error({
    capture_melt_multiple(who, variable=list(
      column="new",
      maybe="_?",
      diagnosis=".*",
      value="_",
      gender=".",
      ages=list(
        min.years="0|[0-9]{2}", as.numeric,
        max.years=list("[0-9]{2}"), "?",
        function(x)ifelse(x=="", Inf, as.numeric(x)))))
  }, "need multiple output columns, but only one value (new) captured in column group; either provide a different regex that captures more than one value in column group, or use capture_melt_single if you really want only one output column", fixed=TRUE)
})


