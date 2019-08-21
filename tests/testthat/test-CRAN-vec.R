library(nc)
library(testthat)
context("variable args syntax")

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)

  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }
  subject <- c(
    ten="chr10:213,054,000-213,055,000",
    chrNA="chrNA:111,000-222,000",
    no.match="foo bar",
    missing=NA,
    two="chr1:110-111 chr2:220-222")
  test_engine("vec_capture_first returns data.frame with chr columns", {
    computed <- vec_capture_first(
      subject,
      chrom="chr.*?",
      ":",
      chromStart=".*?",
      "-",
      chromEnd="[0-9,]*",
      nomatch.error=FALSE)
    expected <- data.frame(
      chrom=c("chr10", "chrNA", NA, NA, "chr1"),
      chromStart=c("213,054,000", "111,000", NA, NA, "110"),
      chromEnd=c("213,055,000", "222,000", NA, NA, "111"),
      row.names=names(subject),
      stringsAsFactors=FALSE)
    expect_identical(computed, expected)
  })

  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  test_engine("vec_capture_first returns data.frame with int columns", {
    computed <- vec_capture_first(
      subject,
      chrom="chr.*?",
      ":",
      chromStart=".*?", keep.digits,
      "-",
      chromEnd="[0-9,]*", keep.digits,
      nomatch.error=FALSE)
    expected <- data.frame(
      chrom=c("chr10", "chrNA", NA, NA, "chr1"),
      chromStart=as.integer(c(213054000, 111000, NA, NA, 110)),
      chromEnd=as.integer(c(213055000, 222000, NA, NA, 111)),
      stringsAsFactors=FALSE,
      row.names=names(subject))
    expect_identical(computed, expected)
  })

  test_engine("named function is an error", {
    expect_error({
      vec_capture_first(
        subject,
        chrom="chr.*?",
        ":",
        chromStart=".*?", fun=keep.digits,
        "-",
        chromEnd="[0-9,]*", keep.digits)
    }, "functions must not be named, problem: fun")
  })

  test_engine("vec_capture_first errors for one argument", {
    expect_error({
      vec_capture_first("foo")
    }, "must have at least one named argument")
  })


  test_engine("vec_capture_first errors for multi-dim patterns", {
    expect_error({
      vec_capture_first("foo", bar=c("bar", "baz"))
    }, "patterns must be character vectors of length 1")
  })

  test_engine("vec_capture_first errors for 0-length patterns", {
    expect_error({
      vec_capture_first("foo", bar=character())
    }, "patterns must be character vectors of length 1")
  })

  test_engine("vec_capture_first errors for non char/fun args", {
    expect_error({
      vec_capture_first("foo", baz="bar", 1)
    }, "arguments must be", fixed=TRUE)
  })

  test_engine("vec_capture_first errors for two funs in a row", {
    expect_error({
      vec_capture_first(
        "foo", g="bar", as.integer, as.numeric)
    },
    "too many functions; up to one function may follow each named pattern")
  })

  test_engine("vec_capture_first errors for fun at start", {
    expect_error({
      vec_capture_first("foo", as.numeric, bar="baz")
    },
    "too many functions; up to one function may follow each named pattern")
  })

  test_engine("vec_capture_first errors for NA pattern", {
    expect_error({
      vec_capture_first("foo", g="bar", NA_character_, "baz")
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
  test_engine("nested lists are OK", {
    task.df <- do.call(vec_capture_first, all.args)
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
  test_engine("vec square brackets pattern", {
    if(engine=="ICU"){
      expect_error({
        vec_capture_first(task.vec, full.square)
      }, "when matching pattern printed above with ICU engine")
    }else{
      task.df <- vec_capture_first(task.vec, full.square)
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
  chr.pos.df <- vec_capture_first(
    chr.pos.nomatch.vec,
    chrom="chr.*?",
    ":",
    chromStart="[0-9,]+", keep.digits,
    list(
      "-",
      chromEnd="[0-9,]+", keep.digits
    ), "?",
    nomatch.error=FALSE)
  test_engine("un-named list interpreted as non-capturing group", {
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
  test_engine("str subject no error if nomatch.error=TRUE and all matches", {
    match.df <- vec_capture_first(
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
  test_engine("str subject stop if nomatch.error=TRUE and no match", {
    expect_error({
      vec_capture_first(
        chr.pos.nomatch.vec, nomatch.error=TRUE,
        chrom="chr.*?",
        ":",
        chromStart="[0-9,]+", keep.digits,
        list(
          "-",
          chromEnd="[0-9,]+", keep.digits
        ), "?")
    }, "subjects printed above did not match regex below")
  })

  (foo.mat <- vec_capture_first(
    c("foo", "foobar", "fooba"),
    first="foo",
    list("b", second="ar"), "?"))
  test_engine("un-named list interpreted as non-capturing group foo subject", {
    expect_identical(foo.mat[, "first"], c("foo", "foo", "foo"))
    expect_identical(foo.mat[, "second"], c("", "ar", ""))
  })

  subject <- "foo55bar"
  test_engine("vec_capture_first returns df with only one group = name", {
    out.df <- vec_capture_first(
      subject,
      name="[0-9]+", as.integer)
    exp.df <- data.frame(row.names="55")
    expect_identical(out.df, exp.df)
  })

}
