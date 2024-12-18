library(testthat)
library(nc)
library(data.table)
context("multiple")
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

test_engines("only one input variable per column value is an error", {
  expect_error({
    nc::capture_melt_multiple(iris, column=".*[.].*", other=".*")
  }, "only one input variable for each value captured in column group; typically this happens when the column group matches the entire input column name; fix by changing regex so that column group matches a strict substring (not the entire input column names)",
  fixed=TRUE)
})

iris.dt <- data.table(i=1:nrow(iris), iris)
iris.dims <- capture_melt_multiple(
  iris.dt,
  part=".*?",
  "[.]",
  column=".*")
iris.dims.wide <- dcast(
  iris.dims,
  i + Species ~ part,
  value.var=c("Length", "Width"))
names(iris.dims.wide) <- sub("(.*?)_(.*)", "\\2.\\1", names(iris.dims.wide))
should.be.orig <- iris.dims.wide[, names(iris.dt), with=FALSE]
setkeyv(should.be.orig, key(iris.dt))
test_engines("orig iris.dt and recast data are the same", {
  expect_equal(should.be.orig, iris.dt)
})

iris.dt[, chr := paste(Species)]
compare.cols <- c(
  "Sepal.Length", "Petal.Length",
  "Sepal.Width", "Petal.Width",
  "chr")
set.seed(1)
iris.rand <- iris.dt[sample(.N)]
iris.wide <- cbind(treatment=iris.rand[1:75], control=iris.rand[76:150])
test_engines("iris multiple column matches", {
  nc.melted <- capture_melt_multiple(
    iris.wide,
    group="[^.]+",
    "[.]",
    column=".*")
  expect_identical(nc.melted$group, rep(c("control", "treatment"), each=75))
  nc.melted.orig <- nc.melted[order(i), ..compare.cols]
  expect_identical(nc.melted.orig, iris.dt[, ..compare.cols])
})

set.seed(1)
iris.wide.rand <- iris.wide[, sample(names(iris.wide)), with=FALSE]
test_engines("iris multiple rand column matches", {
  rand.melted <- capture_melt_multiple(
    iris.wide.rand,
    group="[^.]+",
    "[.]",
    column=".*"
  )[order(i)]
  expect_identical(rand.melted[, ..compare.cols], iris.dt[, ..compare.cols])
})

iris.wide.bad <- data.table(iris.wide)
names(iris.wide.bad)[2] <- "treatment.Sepal.bad"
test_engines("iris multiple rand column matches", {
  expect_error({
    capture_melt_multiple(
      iris.wide.bad,
      group="[^.]+",
      "[.]",
      column=".*")
  }, "need column=same count for each value")
})

set.seed(45)
DT <- data.table(
  i_1 = c(1:5, NA),
  i_2 = c(NA,6:10),
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01"))
## add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]
test_engines("melt multiple column types error if no other group", {
  expect_error({
    capture_melt_multiple(DT, column="^[^c]")
  }, "need at least one group other than column")
})

test_engines("melt multiple column types", {
  result <- capture_melt_multiple(
    DT,
    column="^[^c]",
    "_",
    .number="[0-9]", as.integer)
  expect_identical(result$.number, rep(1:2, each=nrow(DT)))
  expect_is(result$c_1, "character")
  expect_is(result$i, "integer")
  expect_is(result$f, "character")
  expect_is(result$d, "Date")
  expect_is(result$l, "list")
  expect_identical(nrow(result), 12L)
})

family.dt <- fread(text="family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
test_engines("gender dob example", {
  na.children <- capture_melt_multiple(
    family.dt,
    column="[^_]+",
    between="_child",
    number="[1-3]",
    na.rm=FALSE)
  expect_is(na.children$family_id, "integer")
  expect_is(na.children$age_mother, "integer")
  expect_is(na.children$dob, "IDate")
  expect_is(na.children$gender, "integer")
  expect_equal(sum(is.na(na.children$dob)), 4)
  expect_equal(nrow(na.children), 15)
})

test_engines("error for unequal number of children", {
  bad.groups <- data.table(family.dt, dob_child0="1999-01-01", gender_child4=2)
  expect_error({
    capture_melt_multiple(
      bad.groups,
      column="[^_]+",
      between="_child",
      number="[0-9]")
  }, "need between,number=same count for each value")
})

test_engines("error for column conversion to factor", {
  expect_error({
    capture_melt_multiple(
      family.dt,
      column="[^_]+", as.factor,
      between="_child",
      number="[0-9]")
  },
  "column group must be character, but conversion function returned factor")
})

test_engines("error for column conversion to integer", {
  expect_error({
    expect_warning({
      capture_melt_multiple(
        family.dt,
        column="[^_]+", as.integer,
        between="_child",
        number="[0-9]")
    }, "NAs introduced by coercion")
  },
  "column group must be character, but conversion function returned integer")
})

test_engines("error for unequal number of columns", {
  bad.cols <- data.table(family.dt, bar_child0="1999-01-01", foo_child0=2)
  expect_error({
    capture_melt_multiple(
      bad.cols,
      column="[^_]+",
      between="_child",
      number="[0-9]")
  }, "need column=same count for each value")
})

test_engines("gender dob example na.rm=TRUE", {
  children <- capture_melt_multiple(
    family.dt,
    column="[^_]+",
    between="_child",
    number="[1-3]",
    na.rm=TRUE)
  expect_is(children$family_id, "integer")
  expect_is(children$age_mother, "integer")
  expect_is(children$dob, "IDate")
  expect_is(children$gender, "integer")
  expect_equal(sum(is.na(children$dob)), 0)
  expect_equal(nrow(children), 11)
})

test_engines("variable group ok in melt_multiple", {
  result <- capture_melt_multiple(
    family.dt,
    column="[^_]+",
    variable="_child",
    number="[1-3]")
  exp.names <- c(
    "family_id", "age_mother",
    "variable", "number",
    "dob", "gender")
  expect_identical(names(result), exp.names)
})

test_engines("multiple error if subject not df", {
  expect_error({
    capture_melt_multiple("foobar")
  }, "first argument (subject) must be a data.frame", fixed=TRUE)
})

test_engines("multiple error if no arg named variable", {
  expect_error({
    capture_melt_multiple(family.dt, baz="family")
  }, "pattern must define group named column")
})

test_engines("multiple error if no matching column names", {
  expect_error({
    capture_melt_multiple(family.dt, column="foobar")
  }, "no column names match regex")
})

## what if input df has repeated names?
bad.dt <- data.table(family_id=LETTERS[1:5], family.dt)
test_engines("multiple df with same col names is an error", {
  expect_error({
    capture_melt_multiple(
      bad.dt, column=".*", "_", field("child", "", "[0-9]"))
  }, "input must have columns with unique names, problems: family_id")
})

## what if there are two groups with the same name?
test_engines("multiple groups with the same name is an error", {
  expect_error({
    capture_melt_multiple(
      family.dt, column=".*", child="_", field("child", "", "[0-9]"))
  }, "duplicate capture group names are only allowed in alternatives, problem: child")
})

## what if a capture group has the same name as an input column?
test_engines("err mult capture group same as input col", {
  expect_error({
    capture_melt_multiple(
      family.dt, column=".*", family_id="_", field("child", "", "[0-9]"))
  },
  "some capture group names (family_id) are the same as input column names that did not match the pattern; please change either the pattern or the capture group names so that all output column names will be unique",
  fixed=TRUE)
})

## what if value.name is the same as input col?
bad2 <- data.table(family.dt)
names(bad2)[1] <- "dob"
test_engines("err change value.name if same as input col", {
  expect_error({
    capture_melt_multiple(
      bad2, column=".*", family_id="_", field("child", "", "[0-9]"))
  },
  "unable to create unique output column names; some values (dob) captured by the regex group named column are the same as input column names which do not match the pattern; please change either the pattern or the input column names which do not match the pattern so that output column names will be unique",
  fixed=TRUE)
})

test_engines("err mult value.name same as group names", {
  expect_error({
    capture_melt_multiple(
      family.dt, column=".*", dob="_", field("child", "", "[0-9]"))
  },
  "unable to create unique output column names; some values (dob) captured by the regex group named column are the same as other regex group names; please change either the pattern or the other regex group names so that output column names will be unique",
  fixed=TRUE)
})

i.vec <- 1:10000
c.vec <- paste(i.vec)
one.row <- data.frame(I=t(i.vec), C=t(c.vec), stringsAsFactors=FALSE)
test_engines("multiple melting lots of columns is OK", {
  out <- capture_melt_multiple(
    one.row, column=".", "[.]", int="[0-9]+", as.integer)
  expect_identical(out$int, i.vec)
  expect_identical(out$I, i.vec)
  expect_identical(out$C, c.vec)
})

test_engines("multiple melting lots of columns type.convert=TRUE is OK", {
  out <- capture_melt_multiple(
    one.row, column=".", "[.]", int="[0-9]+", type.convert=TRUE)
  expect_identical(out$int, i.vec)
  expect_identical(out$I, i.vec)
  expect_identical(out$C, c.vec)
})

wide.metrics <- data.table(
  FP.possible=8202, FN.possible=1835,
  FP.count=0, FN.count=1835)
test_engines("count is either 0 or 1835", {
  tall.metrics <- capture_melt_multiple(
    wide.metrics,
    metric=".*?",
    "[.]",
    column=".*")[order(metric)]
  expect_identical(tall.metrics$metric, c("FN", "FP"))
  expect_identical(tall.metrics$count, c(1835, 0))
  expect_identical(tall.metrics$possible, c(1835, 8202))
})

test_engines("melt multiple with count group ok", {
  iris.count <- nc::capture_melt_multiple(
    iris,
    column=".*?",
    "[.]",
    count=".*")
  expect_identical(names(iris.count), c("Species", "count", "Petal", "Sepal"))
})

## apparently there are some data with missing columns
## https://github.com/Rdatatable/data.table/issues/4027
wide.input <- data.table(
  id = 1, a.1 = 1, a.3 = 3, b.1 = 1, b.2 = 2, b.3 = 3)
ix.pattern <- list(column="[a-z]", "[.]", ix="[0-9]", as.integer)
test_engines("error by default for missing column", {
  expect_error({
    capture_melt_multiple(wide.input, ix.pattern)
  }, "need ix=same count for each value, but have: 1=2 2=1 3=2; please change pattern, edit input column names, or use fill=TRUE to output missing values")
})

##But what I really want is the ix of a.3 to be 3, not 2.
##    id ix  a b
## 1:  1  1  1 1
## 2:  1  2 NA 2
## 3:  1  3  3 3
tall.expected <- data.table(
  id=1,
  ix=1:3,
  a=c(1, NA, 3),
  b=1:3)
test_engines("NA for missing column when fill=TRUE", {
  tall.output <- capture_melt_multiple(
    wide.input, ix.pattern, fill=TRUE)
  expect_equal(data.frame(tall.output), data.frame(tall.expected))
})

test_engines("NA for missing columns as in data table example", {
  DT.missing.cols <- DT[, .(d_1, d_2, c_1, f_2)]
  computed <- capture_melt_multiple(
    DT.missing.cols,
    column="[a-z]",
    "_",
    int="[0-9]", as.integer,
    fill=TRUE)
  expected <- DT.missing.cols[, data.table(
    int=rep(1:2, each=.N),
    c=c(c_1, rep(NA, .N)),
    d=c(d_1, d_2),
    f=c(rep(NA, .N), paste(f_2)))]
  expect_identical(data.frame(computed), data.frame(expected))
})

family4.dt <- data.table(family.dt)
family4.dt[, `:=`(dob_child4=NA_character_, gender_child4=NA_integer_)]
test_engines("no families with 4 children", {
  child.pattern <- list(
    column="[^_]+",
    between="_child",
    number="[1-4]")
  na.rm.T <- suppressWarnings({
    capture_melt_multiple(
      family4.dt,
      child.pattern,
      na.rm=TRUE)
  })
  expect_equal(sum(is.na(na.rm.T$dob)), 0)
})

test_engines("lots of missing columns ok with chr capture col small", {
  tall.dt <- data.table(
    family=1,
    child=10:1,
    num=c(10, 9, rep(NA, 8)),
    chr=c("ten", "nine", rep(NA, 8)))
  wide.dt <- dcast(
    tall.dt,
    family ~ child,
    value.var=c("num", "chr"))
  tall.again <- capture_melt_multiple(
    wide.dt,
    column=".*",
    "_",
    child="[0-9]+",
    na.rm=TRUE)[order(num)]
  expect_identical(tall.again$child, paste(9:10))
})

test_engines("lots of missing columns ok with chr capture col big", {
  PROVEDIt.csv <- system.file(
    "extdata", "RD12-0002_PP16HS_5sec_GM_F_1P.csv",
    package="nc", mustWork=TRUE)
  PROVEDIt.wide <- data.table::fread(PROVEDIt.csv)
  PROVEDIt.tall <- suppressWarnings({
    nc::capture_melt_multiple(
      PROVEDIt.wide,
      column=".*",
      " ",
      peak="[0-9]+",
      na.rm=TRUE)
  })
  peak.int <- sort(as.integer(unique(PROVEDIt.tall$peak)))
  expect_identical(peak.int, seq_along(peak.int))
})

test_engines("lots of missing columns chr col big na.rm=FALSE", {
  PROVEDIt.csv <- system.file(
    "extdata", "RD12-0002_PP16HS_5sec_GM_F_1P.csv",
    package="nc", mustWork=TRUE)
  PROVEDIt.wide <- data.table::fread(PROVEDIt.csv)
  PROVEDIt.tall <- suppressWarnings({
    nc::capture_melt_multiple(
      PROVEDIt.wide,
      column=".*",
      " ",
      peak="[0-9]+",
      na.rm=FALSE)[!is.na(Size)]
  })
  ## Why does this test pass? No missing values are removed so data
  ## table assigns peak numbers 1-100 correctly and then we filter the
  ## NAs.
  peak.int <- sort(as.integer(unique(PROVEDIt.tall$peak)))
  expect_identical(peak.int, seq_along(peak.int))
})

test_engines("lots of missing columns int col big na.rm=TRUE", {
  PROVEDIt.csv <- system.file(
    "extdata", "RD12-0002_PP16HS_5sec_GM_F_1P.csv",
    package="nc", mustWork=TRUE)
  PROVEDIt.wide <- data.table::fread(PROVEDIt.csv)
  PROVEDIt.tall <- suppressWarnings({
    nc::capture_melt_multiple(
      PROVEDIt.wide,
      column=".*",
      " ",
      peak="[0-9]+", as.integer,
      na.rm=TRUE)
  })
  ## Why does this test pass? With buggy data.table the missing values
  ## are removed and then the peak IDs are assigned. Because the
  ## missing values are at the end (for peak>36) and the data are
  ## sorted numerically by integers, the assigned IDs match the peak
  ## numbers exactly.
  peak.int <- sort(as.integer(unique(PROVEDIt.tall$peak)))
  expect_identical(peak.int, seq_along(peak.int))
})

test_engines("missing first child", {
  wide.dt <- data.table(
    family=1,
    age_childA=NA_real_, sex_childA=NA_character_,
    age_childB=5, sex_childB="m")
  child.pattern <- list(
    column=".*",
    "_",
    nc::field("child", "", "[A-Z]"))
  tall.dt <- nc::capture_melt_multiple(wide.dt, child.pattern, na.rm=TRUE)
  expect_identical(tall.dt$child, "B")
  expect_identical(tall.dt$age, 5)
  expect_identical(tall.dt$sex, "m")
})
