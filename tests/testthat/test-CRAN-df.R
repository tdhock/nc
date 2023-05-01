library(nc)
library(testthat)
context("df")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_engines("default/specified engine respected", {
  subject.df <- data.frame(f="foo", b="bar")
  match.dt <- capture_first_df(
    subject.df,
    f=list(icu="[\\p{Letter}]"),#only works with ICU
    b=list(re2=".(?R)?", engine="PCRE"),#only works with PCRE
    engine="ICU")
  expect_identical(match.dt[["icu"]], "f")
  expect_identical(match.dt[["re2"]], "bar")
})

subject.df <- data.frame(
  JobID=c(
    "13937810_25",
    "13937810_25.batch",
    "13937810_25.extern",
    "14022192_[1-3]",
    "14022204_[4]"),
  subject=c(
    ten="chr10:213,054,000-213,055,000",
    chrNA="chrNA:111,000-222,000",
    no.match="foo bar",
    missing=NA,
    two="chr1:110-111 chr2:220-222"),
  stringsAsFactors=FALSE)
range.pattern <- list(
  "\\[",
  task1="[0-9]+", as.integer,
  "(?:-",#begin optional end of range.
  taskN="[0-9]+", as.integer,
  ")?", #end is optional.
  "\\]")
test_engines("capture_first_df returns data.table", {
  match.dt <- capture_first_df(
    subject.df,
    JobID=list(
      job="[0-9]+", as.integer,
      "_",
      "(?:",#begin alternate
      task="[0-9]+", as.integer,
      "|",#either one task(above) or range(below)
      range.pattern,
      ")",#end alternate
      "(?:[.]",
      type=".*",
      ")?"),
    subject=list(
      nomatch.error=FALSE,
      chrom="chr.*?",
      ":",
      chromStart=".*?",
      "-",
      chromEnd="[0-9,]*"))
  expect_is(match.dt, "data.table")
  expect_identical(names(match.dt), c(
    "JobID", "subject",
    "job", "task", "task1", "taskN", "type",
    "chrom", "chromStart", "chromEnd"))
  expect_identical(match.dt$job, as.integer(c(
    13937810, 13937810, 13937810, 14022192, 14022204)))
  expect_identical(match.dt$task, as.integer(c(
    25, 25, 25, NA, NA)))
  expect_identical(match.dt$task1, as.integer(c(
    NA, NA, NA, 1, 4)))
  expect_identical(match.dt$taskN, as.integer(c(
    NA, NA, NA, 3, NA)))
  expect_identical(match.dt$type, c(
    "", "batch", "extern", "", ""))
  expect_identical(match.dt$chrom, c(
    "chr10", "chrNA", NA, NA, "chr1"))
  expect_identical(match.dt$chromStart, c(
    "213,054,000", "111,000", NA, NA, "110"))
  expect_identical(match.dt$chromEnd, c(
    "213,055,000", "222,000", NA, NA, "111"))
})

range.square <- list(
  "[[]",
  task1="[0-9]+", as.integer,
  "(?:-",#begin optional end of range.
  taskN="[0-9]+", as.integer,
  ")?", #end is optional.
  "[]]")
test_engines("capture_first_df square brackets pattern", {
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
  if(identical(getOption("nc.engine"), "ICU")){
    expect_error({
      capture_first_df(subject.df, JobID=full.square)
    }, "when matching pattern above with ICU engine")
  }else{
    match.dt <- capture_first_df(subject.df, JobID=full.square)
    expect_identical(names(match.dt), c(
      "JobID", "subject",
      "job", "task", "task1", "taskN", "type"))
    expect_identical(match.dt$job, as.integer(c(
      13937810, 13937810, 13937810, 14022192, 14022204)))
    expect_identical(match.dt$task, as.integer(c(
      25, 25, 25, NA, NA)))
    expect_identical(match.dt$task1, as.integer(c(
      NA, NA, NA, 1, 4)))
    expect_identical(match.dt$taskN, as.integer(c(
      NA, NA, NA, 3, NA)))
    expect_identical(match.dt$type, c(
      "", "batch", "extern", "", ""))
  }
})

no.rownames <- data.frame(
  JobID=c(
    "13937810_25",
    "13937810_25.batch",
    "13937810_25.extern",
    "14022192_[1-3]",
    "14022204_[4]"),
  position=c(
    "chr10:213,054,000-213,055,000",
    "chrNA:111,000-222,000",
    "foo bar",
    NA,
    "chr1:110-111 chr2:220-222"),
  stringsAsFactors=FALSE)
test_engines("capture_first_df returns data.frame with default rownames", {
  match.df <- capture_first_df(
    no.rownames,
    JobID=list(
      job="[0-9]+", as.integer,
      "_",
      "(?:",#begin alternate
      task="[0-9]+", as.integer,
      "|",#either one task(above) or range(below)
      range.pattern,
      ")",#end alternate
      "(?:[.]",
      type=".*",
      ")?"),
    position=list(
      nomatch.error=FALSE,
      chrom="chr.*?",
      ":",
      chromStart=".*?",
      "-",
      chromEnd="[0-9,]*"))
  expect_identical(names(match.df), c(
    "JobID", "position",
    "job", "task", "task1", "taskN", "type",
    "chrom", "chromStart", "chromEnd"))
  expect_identical(match.df$job, as.integer(c(
    13937810, 13937810, 13937810, 14022192, 14022204)))
  expect_identical(match.df$task, as.integer(c(
    25, 25, 25, NA, NA)))
  expect_identical(match.df$task1, as.integer(c(
    NA, NA, NA, 1, 4)))
  expect_identical(match.df$taskN, as.integer(c(
    NA, NA, NA, 3, NA)))
  expect_identical(match.df$type, c(
    "", "batch", "extern", "", ""))
  expect_identical(match.df$chrom, c(
    "chr10", "chrNA", NA, NA, "chr1"))
  expect_identical(match.df$chromStart, c(
    "213,054,000", "111,000", NA, NA, "110"))
  expect_identical(match.df$chromEnd, c(
    "213,055,000", "222,000", NA, NA, "111"))
  expect_identical(rownames(match.df), paste(1:5))
})

named.uniq.chr <- data.frame(
  JobID=c(
    foo="13937810_25",
    bar="13937810_25.batch",
    baz="13937810_25.extern",
    sars="14022192_[1-3]",
    last="14022204_[4]"),
  position=c(
    "chr10:213,054,000-213,055,000",
    "chrNA:111,000-222,000",
    "chr2:1-2",
    "chr3:4-5",
    "chr1:110-111 chr2:220-222"),
  stringsAsFactors=FALSE)
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
test_engines("two name groups not OK with named subject", {
  expect_error({
    capture_first_df(
      named.uniq.chr,
      JobID=list(
        name="[^.]+[.].|[0-9]+",
        rest=".*"),
      position=list(
        name="chr.*?",
        ":",
        chromStart=".*?", keep.digits,
        "-",
        chromEnd="[0-9,]*", keep.digits))
  }, "must not conflict with existing column names")
})

test_engines("error for no pattern", {
  expect_error({
    capture_first_df(named.uniq.chr)
  }, "no patterns specified in ...")
})

test_engines("error for un-named list", {
  expect_error({
    capture_first_df(named.uniq.chr, list())
  }, "each pattern in ... must be named using a column name of subject")
})

test_engines("error for un-named list with name", {
  expect_error({
    capture_first_df(named.uniq.chr, list(foo="bar"))
  }, "each pattern in ... must be named using a column name of subject")
})

test_engines("error for un-recognized name", {
  expect_error({
    capture_first_df(named.uniq.chr, foo="bar")
  }, "each pattern in ... must be named using a column name of subject")
})

test_engines("error for non-df subject", {
  expect_error({
    capture_first_df(c("foo", "bar"), list(foo="bar"))
  }, "subject must be a data.frame with character columns to match")
})

test_engines("error for non-df subject", {
  expect_error({
    capture_first_df(c("foo", "bar"), foo="bar")
  }, "subject must be a data.frame with character columns to match")
})

test_engines("error for factor column", {
  fac.df <- data.frame(foo="bar", stringsAsFactors=TRUE)
  expect_error({
    capture_first_df(fac.df, foo=list(baz="sars"))
  }, "problem for subject column foo: Error in stop_for_subject(subject): subject has class=factor and length=1 but should be a character vector with length>0", fixed=TRUE)
})

test_engines("error for same column name twice", {
  expect_error({
    capture_first_df(
      named.uniq.chr,
      JobID=list(f="baz"),
      JobID="foo")
  }, "each argument / subject column name should be unique")
})

test_engines("error for named subject", {
  expect_error({
    capture_first_df(
      JobID=named.uniq.chr,
      JobID=list(f="[0-9]+"))
  },
  "first argument (subject data.frame) should not be named",
  fixed=TRUE)
})

in.df <- data.frame(bar="foobar", stringsAsFactors=FALSE)
test_engines("df only one group = name", {
  out.dt <- capture_first_df(
    in.df,
    bar=list(
      name="foo"))
  expect_identical(out.dt$name, "foo")
})

matching.subjects <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000",
  "chr1:110-111 chr2:220-222") # two possible matches.
test_engines("df subject no error if nomatch.error=TRUE and all matches", {
  subject.df <- data.frame(
    subject.col=matching.subjects, stringsAsFactors=FALSE)
  match.dt <- capture_first_df(
    subject.df,
    subject.col=list(
      nomatch.error=TRUE,
      chrom="chr.*?",
      ":",
      chromStart="[0-9,]+", keep.digits,
      list(
        "-",
        chromEnd="[0-9,]+", keep.digits
      ), "?"))
  expect_identical(
    match.dt$chromEnd,
    as.integer(c(213055000, NA, 111)))
})
chr.pos.nomatch.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
test_engines("df subject stop if nomatch.error=TRUE and no match", {
  subject.df <- data.frame(chr.pos.nomatch.vec, stringsAsFactors=FALSE)
  expect_error({
    capture_first_df(
      subject.df,
      chr.pos.nomatch.vec=list(
        nomatch.error=TRUE,
        chrom="chr.*?",
        ":",
        chromStart="[0-9,]+", keep.digits,
        list(
          "-",
          chromEnd="[0-9,]+", keep.digits
        ), "?"))
  }, "subject(s) 3,4 (2 total) did not match regex below", fixed=TRUE)
})

