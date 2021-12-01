library(nc)
library(testthat)
library(data.table)
context("variable args syntax")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

## In stringi there is a difference between matching an empty
## string, and an optional match that fails.
inames <- c("Species", "Petal.Length", "Sepal.Width")
sapply(c("[^.]*(.*)", "[^.]*([.].*)?"), function(pat){
  stringi::stri_match_first_regex(inames, pat)
})

test_engines("match nothing in required group is empty string", {
  empty.dt <- capture_first_vec(
    inames,
    "[^.]*",
    rest=".*")
  expect_identical(empty.dt, data.table(rest=c("", ".Length", ".Width")))
})

test_engines("no match for optional group is empty string", {
  opt.empty.dt <- capture_first_vec(
    inames,
    "[^.]*",
    rest="[.].*", "?")
  expect_identical(opt.empty.dt, data.table(rest=c("", ".Length", ".Width")))
})

test_engines("no match for required group is NA", {
  na.dt <- capture_first_vec(
    inames,
    "[^.]*",
    rest="[.].*",
    nomatch.error=FALSE)
  expect_identical(na.dt, data.table(rest=c(NA, ".Length", ".Width")))
})

test_engines("error for regex with literal groups", {
  expect_error({
    capture_first_vec(
      c("chr1:100-200", "chr2:5-6"),
      chrom="chr.",
      ":",
      "([0-9]+)")
  }, "regex contains more groups than names; please remove literal groups (parentheses) from the regex pattern, and use named arguments in R code instead", fixed=TRUE)
})

subject <- c(
  ten="chr10:213,054,000-213,055,000",
  chrNA="chrNA:111,000-222,000",
  no.match="foo bar",
  missing=NA,
  two="chr1:110-111 chr2:220-222")
test_engines("capture_first_vec returns data.table with chr columns", {
  computed <- capture_first_vec(
    subject,
    chrom="chr.*?",
    ":",
    chromStart=".*?",
    "-",
    chromEnd="[0-9,]*",
    nomatch.error=FALSE)
  expected <- data.table(
    chrom=c("chr10", "chrNA", NA, NA, "chr1"),
    chromStart=c("213,054,000", "111,000", NA, NA, "110"),
    chromEnd=c("213,055,000", "222,000", NA, NA, "111"))
  expect_identical(computed, expected)
})

keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
test_engines("capture_first_vec returns data.table with int columns", {
  computed <- capture_first_vec(
    subject,
    chrom="chr.*?",
    ":",
    chromStart=".*?", keep.digits,
    "-",
    chromEnd="[0-9,]*", keep.digits,
    nomatch.error=FALSE)
  expected <- data.table(
    chrom=c("chr10", "chrNA", NA, NA, "chr1"),
    chromStart=as.integer(c(213054000, 111000, NA, NA, 110)),
    chromEnd=as.integer(c(213055000, 222000, NA, NA, 111)))
  expect_identical(computed, expected)
})

test_engines("named function is an error", {
  expect_error({
    capture_first_vec(
      subject,
      chrom="chr.*?",
      ":",
      chromStart=".*?", fun=keep.digits,
      "-",
      chromEnd="[0-9,]*", keep.digits)
  }, "functions must not be named, problem: fun")
})

test_engines("capture_first_vec errors for one argument", {
  expect_error({
    capture_first_vec("foo")
  }, "must have at least one named argument")
})


test_engines("capture_first_vec errors for multi-dim patterns", {
  expect_error({
    capture_first_vec("foo", bar=c("bar", "baz"))
  }, "patterns must be character vectors of length 1")
})

test_engines("capture_first_vec errors for 0-length patterns", {
  expect_error({
    capture_first_vec("foo", bar=character())
  }, "patterns must be character vectors of length 1")
})

test_engines("capture_first_vec errors for non char/fun args", {
  expect_error({
    capture_first_vec("foo", baz="bar", 1)
  }, "arguments must be", fixed=TRUE)
})

test_engines("capture_first_vec errors for two funs in a row", {
  expect_error({
    capture_first_vec(
      "foo", g="bar", as.integer, as.numeric)
  },
  "too many functions; up to one function may follow each named pattern")
})

test_engines("capture_first_vec errors for fun at start", {
  expect_error({
    capture_first_vec("foo", as.numeric, bar="baz")
  },
  "too many functions; up to one function may follow each named pattern")
})

test_engines("capture_first_vec errors for NA pattern", {
  expect_error({
    capture_first_vec("foo", g="bar", NA_character_, "baz")
  }, "patterns must not be missing/NA")
})

range.pattern <- list(
  "\\[",
  task1="[0-9]+", as.integer,
  "(?:-",#begin optional end of range.
  taskN="[0-9]+", as.integer,
  ")?", #end is optional.
  "\\]")
full.pattern <- list(
  job="[0-9]+", as.integer,
  "_",
  "(?:",#begin alternate
  task="[0-9]+", as.integer,
  "|",#either one task(above) or range(below)
  range.pattern,
  ")",#end alternate
  "(?:[.]",
  type=".*",
  ")?")
task.vec <- c(
  "13937810_25",
  "13937810_25.batch",
  "13937810_25.extern",
  "14022192_[1-3]",
  "14022204_[4]")
all.args <- list(task.vec, full.pattern)
test_engines("nested lists are OK", {
  task.df <- do.call(capture_first_vec, all.args)
  expect_identical(
    names(task.df),
    c("job", "task", "task1", "taskN", "type"))
  expect_identical(task.df$job, as.integer(c(
    13937810, 13937810, 13937810, 14022192, 14022204)))
  expect_identical(task.df$task, as.integer(c(
    25, 25, 25, NA, NA)))
  expect_identical(task.df$task1, as.integer(c(
    NA, NA, NA, 1, 4)))
  expect_identical(task.df$taskN, as.integer(c(
    NA, NA, NA, 3, NA)))
  expect_identical(task.df$type, c(
    "", "batch", "extern", "", ""))
})

range.square <- list(
  "[[]",
  task1="[0-9]+", as.integer,
  "(?:-",#begin optional end of range.
  taskN="[0-9]+", as.integer,
  ")?", #end is optional.
  "[]]")
full.square <- list(
  job="[0-9]+", as.integer,
  "_",
  "(?:",#begin alternate
  task="[0-9]+", as.integer,
  "|",#either one task(above) or range(below)
  range.square,
  ")",#end alternate
  "(?:[.]",
  type=".*",
  ")?")
test_engines("vec square brackets pattern", {
  if(identical(getOption("nc.engine"), "ICU")){
    expect_error({
      capture_first_vec(task.vec, full.square)
    }, "when matching pattern above with ICU engine")
  }else{
    task.df <- capture_first_vec(task.vec, full.square)
    expect_identical(
      names(task.df),
      c("job", "task", "task1", "taskN", "type"))
    expect_identical(task.df$job, as.integer(c(
      13937810, 13937810, 13937810, 14022192, 14022204)))
    expect_identical(task.df$task, as.integer(c(
      25, 25, 25, NA, NA)))
    expect_identical(task.df$task1, as.integer(c(
      NA, NA, NA, 1, 4)))
    expect_identical(task.df$taskN, as.integer(c(
      NA, NA, NA, 3, NA)))
    expect_identical(task.df$type, c(
      "", "batch", "extern", "", ""))
  }
})

chr.pos.nomatch.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
chr.pos.df <- capture_first_vec(
  chr.pos.nomatch.vec,
  chrom="chr.*?",
  ":",
  chromStart="[0-9,]+", keep.digits,
  list(
    "-",
    chromEnd="[0-9,]+", keep.digits
  ), "?",
  nomatch.error=FALSE)
test_engines("un-named list interpreted as non-capturing group", {
  expect_identical(
    chr.pos.df$chromStart,
    as.integer(c(213054000, 111000, NA, NA, 110)))
  expect_identical(
    chr.pos.df$chromEnd,
    as.integer(c(213055000, NA, NA, NA, 111)))
})

matching.subjects <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000",
  "chr1:110-111 chr2:220-222") # two possible matches.
test_engines("str subject no error if nomatch.error=TRUE and all matches", {
  match.df <- capture_first_vec(
    matching.subjects, nomatch.error=TRUE,
    chrom="chr.*?",
    ":",
    chromStart="[0-9,]+", keep.digits,
    list(
      "-",
      chromEnd="[0-9,]+", keep.digits
    ), "?")
  expect_identical(
    match.df$chromEnd,
    as.integer(c(213055000, NA, 111)))
})
test_engines("str subject stop if nomatch.error=TRUE and no match", {
  expect_error({
    capture_first_vec(
      chr.pos.nomatch.vec, nomatch.error=TRUE,
      chrom="chr.*?",
      ":",
      chromStart="[0-9,]+", keep.digits,
      list(
        "-",
        chromEnd="[0-9,]+", keep.digits
      ), "?")
  }, "subject(s) 3,4 (2 total) did not match regex below", fixed=TRUE)
})

test_that("only a few subject IDs in error", {
  expect_error({
    capture_first_vec(paste(1:1000), foo="bar")
  }, "subject(s) 1,2,3,4,5,...,996,997,998,999,1000 (1000 total) did not match regex below",
  fixed=TRUE)
})

(foo.mat <- capture_first_vec(
  c("foo", "foobar", "fooba"),
  first="foo",
  list("b", second="ar"), "?"))
test_engines("un-named list interpreted as non-capturing group foo subject", {
  expect_identical(foo.mat$first, c("foo", "foo", "foo"))
  expect_identical(foo.mat$second, c("", "ar", ""))
})

subject <- "foo55bar"
test_engines("capture_first_vec returns dt with only one group = name", {
  out.dt <- capture_first_vec(
    subject,
    name="[0-9]+", as.integer)
  expect_equal(dim(out.dt), c(1, 1))
  expect_identical(names(out.dt), "name")
})
