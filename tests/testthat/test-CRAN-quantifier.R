library(nc)
library(testthat)
context("quantifier")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_that("error for no args to quantifier", {
  expect_error({
    quantifier()
  },
  "quantifier needs at least two arguments (patterns, quantifier)",
  fixed=TRUE)
})

test_that("error for one arg to quantifier", {
  expect_error({
    quantifier("foo")
  },
  "quantifier needs at least two arguments (patterns, quantifier)",
  fixed=TRUE)
})

test_that("error for named last arg", {
  expect_error({
    quantifier("a", "b", foo="?")
  }, "last argument to quantifier must be un-named")
})

test_that("error for non-string last arg", {
  expect_error({
    quantifier("a", "b", identity)
  },
  "last argument to quantifier must be character string (quantifier such as ?, *, or {0,2})",
  fixed=TRUE)
})

## Above we use test_that (does not do any matching) and below we do
## test_engines (matches using all regex engines).

test_engines("user-defined optional is ok", {
  job.vec <- readLines(textConnection("26534569
26534569.extern
26534569.0
26534608_63
26534608_63.batch
26534608_63.extern
26534685_[1-373]
26534686_[1]"))
  ## zero_or_one zero_or_more like rex etc?
  optional <- function(...){
    quantifier(..., "?")
  }
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
  match.dt <- capture_first_vec(
    job.vec,
    job="[0-9]+", as.integer,
    optional.task,
    optional.type)
  expect_equal(nrow(match.dt), length(job.vec))
  expect_false(any(is.na(match.dt$job)))
})

test_engines("quantifier works for non-greedy lines", {
  vignette.Rmd <- system.file(
    "extdata", "vignette.Rmd", package="nc")
  non.greedy.lines <- quantifier(".*\n", "*?")
  Rmd.dt <- capture_all_str(
    vignette.Rmd,
    before=non.greedy.lines,
    "```\\{r",
    quantifier(" ", name="[^,}]+", "?"),
    parameters=".*",
    "\\}\n",
    code=non.greedy.lines,
    "```")
  expect_equal(nrow(Rmd.dt), 14)
})


