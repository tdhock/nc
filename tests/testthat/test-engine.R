library(nc)
library(testthat)
context("engine")

test_that("error for unknown engine", {
  expect_error({
    capture_first_vec("foo", bar="foo", engine="sars")
  }, "engine must be character string")
})

expect_error_engine_pkg <- function(engine, pkg){
  if(isNamespaceLoaded(pkg))unloadNamespace(pkg)
  pkg.dir <- system.file(package=pkg)
  new.dir <- paste0(pkg.dir, "_tmp")
  file.rename(pkg.dir, new.dir)
  expect_error({
    capture_first_vec("foo", bar="foo", engine=engine)
  }, pkg)
  file.rename(new.dir, pkg.dir)
}
test_that("error tells user to install re2r when RE2 not available", {
  expect_error_engine_pkg("RE2", "re2r")
})
test_that("error tells user to install stringi when ICU not available", {
  expect_error_engine_pkg("ICU", "stringi")
})

