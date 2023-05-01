library(testthat)
library(nc)
library(data.table)
context("measure")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

if("measure" %in% ls(asNamespace("data.table"))){

  DT <- data.table(a_1=10, b_2=21, a_2=20)
  test_engines("measure single value", {
    out <- melt(DT, measure.vars=nc::measure(
      letter="[ab]", "_", number="[12]", as.integer))
    expected <- data.table(
      letter=c("a","b","a"),
      number=as.integer(c(1,2,2)),
      value=c(10,21,20))
    expect_identical(out, expected)
  })

  test_engines("measure multiple values", {
    out <- melt(DT, measure.vars=nc::measure(
      column="[ab]", "_", number="[12]", as.integer))
    expected <- data.table(
      number=as.integer(c(1,2)),
      a=c(10,20),
      b=c(NA,21))
    expect_identical(out, expected)
  })

}
