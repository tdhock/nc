library(nc)
library(testthat)
context("optional")
job.vec <- readLines(textConnection("26534569
26534569.extern
26534569.0
26534608_63
26534608_63.batch
26534608_63.extern
26534685_[1-373]
26534686_[1]"))
optional <- function(...){
  list(list(...), "?")
}
## zero_or_one zero_or_more like rex etc?
optional.end <- optional("-", taskN="[0-9]+", as.integer)
range.pattern <- list(
  "\\[",
  task1="[0-9]+", as.integer,
  optional.end,
  "\\]")
task.pattern <- list(
  task="[0-9]+", as.integer,
  "|",#either one task(above) or range(below)
  range.pattern)
optional.type <- optional("[.]", type=".*")
optional.task <- optional("_", task.pattern)

test_that("user-defined optional is ok", {
  match.dt <- capture_first_vec(
    job.vec,
    job="[0-9]+", as.integer,
    optional.task,
    optional.type)
  expect_equal(nrow(match.dt), length(job.vec))
  expect_false(any(is.na(match.dt$job)))
})
