library(testthat)
library(nc)
context("multiple")
library(data.table)

for(engine in c("PCRE", "RE2", "ICU")){
  options(nc.engine=engine)
  test_engine <- function(msg, ...){
    test_that(paste(engine, msg), ...)
  }

  iris.dt <- data.table(i=1:nrow(iris), iris)
  iris.dims <- capture_first_melt_multiple(
    iris.dt,
    part=".*?",
    "[.]",
    column=".*")
  iris.dims.wide <- dcast(iris.dims, i + Species ~ part, value.var=c("Length", "Width"))
  names(iris.dims.wide) <- sub("(.*?)_(.*)", "\\2.\\1", names(iris.dims.wide))
  should.be.orig <- iris.dims.wide[, names(iris.dt), with=FALSE]
  setkeyv(should.be.orig, key(iris.dt))
  test_engine("orig iris.dt and recast data are the same", {
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
  test_engine("iris multiple column matches", {
    nc.melted <- capture_first_melt_multiple(
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
  test_engine("iris multiple rand column matches", {
    rand.melted <- capture_first_melt_multiple(
      iris.wide.rand,
      group="[^.]+",
      "[.]",
      column=".*"
    )[order(i)]
    expect_identical(rand.melted[, ..compare.cols], iris.dt[, ..compare.cols])
  })

  iris.wide.bad <- data.table(iris.wide)
  names(iris.wide.bad)[2] <- "treatment.Sepal.bad"
  test_engine("iris multiple rand column matches", {
    expect_error({
      capture_first_melt_multiple(
        iris.wide.bad,
        group="[^.]+",
        "[.]",
        column=".*")
    }, "need same number of values for each column")
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
  test_engine("melt multiple column types error if no other group", {
    expect_error({
      capture_first_melt_multiple(DT, column="^[^c]")
    }, "need at least one group other than column")
  })

  test_engine("melt multiple, int id matches regex is an error", {
    expect_error({
      capture_first_melt_multiple(
        DT,
        column="[^c]",
        "_",
        number="[1-2]",
        id.vars=3:5)
    }, "some id.vars (f_1, f_2) matched the regex below, but should not", fixed=TRUE)
  })

  test_engine("melt multiple, chr id matches regex is an error", {
    expect_error({
      capture_first_melt_multiple(
        DT,
        column="[^c]",
        "_",
        number="[1-2]",
        id.vars=c("c_1", "f_1", "f_2"))
    }, "some id.vars (f_1, f_2) matched the regex below, but should not", fixed=TRUE)
  })

  test_engine("melt multiple column types without id.vars", {
    result <- capture_first_melt_multiple(
      DT,
      column="^[^c]",
      "_",
      number="[0-9]")
    expect_is(result$c_1, "character")
    expect_is(result$i, "integer")
    expect_is(result$f, "character")
    expect_is(result$d, "Date")
    expect_is(result$l, "list")
    expect_identical(nrow(result), 12L)
  })

  test_engine("melt multiple column types with id.vars", {
    id.result <- capture_first_melt_multiple(
      DT,
      column="^[fl]",
      "_",
      number="[0-9]",
      id.vars=1:2)
    expect_is(id.result$i_1, "integer")
    expect_is(id.result$i_2, "integer")
    expect_is(id.result$f, "character")
    expect_is(id.result$l, "list")
    expect_identical(nrow(id.result), 12L)
  })

  D2 <- fread(text="family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
  test_engine("gender dob example", {
    na.children <- capture_first_melt_multiple(
      D2,
      column="[^_]+",
      between="_child",
      number="[1-3]")
    expect_is(na.children$family_id, "integer")
    expect_is(na.children$age_mother, "integer")
    expect_is(na.children$dob, "character")
    expect_is(na.children$gender, "integer")
    expect_equal(sum(is.na(na.children$dob)), 4)
    expect_equal(nrow(na.children), 15)
  })

  test_engine("error for unequal number of children", {
    bad.groups <- data.table(D2, dob_child0="1999-01-01", gender_child4=2)
    expect_error({
      capture_first_melt_multiple(
        bad.groups,
        column="[^_]+",
        between="_child",
        number="[0-9]")
    }, "need same number of values for each group")
  })

  test_engine("error for unequal number of columns", {
    bad.cols <- data.table(D2, bar_child0="1999-01-01", foo_child0=2)
    expect_error({
      capture_first_melt_multiple(
        bad.cols,
        column="[^_]+",
        between="_child",
        number="[0-9]")
    }, "need same number of values for each column")
  })

  test_engine("gender dob example na.rm=TRUE", {
    children <- capture_first_melt_multiple(
      D2,
      column="[^_]+",
      between="_child",
      number="[1-3]",
      na.rm=TRUE)
    expect_is(children$family_id, "integer")
    expect_is(children$age_mother, "integer")
    expect_is(children$dob, "character")
    expect_is(children$gender, "integer")
    expect_equal(sum(is.na(children$dob)), 0)
    expect_equal(nrow(children), 11)
  })

  test_engine("error for .col.i arg in melt_multiple", {
    expect_error({
      capture_first_melt_multiple(
        D2,
        column="[^_]+",
        .col.i="_child",
        number="[1-3]")
    }, "dot (.) must not be used at the start of an argument/group name, problems: .col.i", fixed=TRUE)
  })

  test_engine("variable group ok in melt_multiple", {
    result <- capture_first_melt_multiple(
      D2,
      column="[^_]+",
      variable="_child",
      number="[1-3]")
    exp.names <- c(
      "variable", "number",
      "family_id", "age_mother",
      "dob", "gender")
    expect_identical(names(result), exp.names)
  })

  test_engine("error for .variable arg in melt_multiple", {
    expect_error({
      capture_first_melt_multiple(
        D2,
        column="[^_]+",
        .variable="_child",
        number="[1-3]")
    }, "dot (.) must not be used at the start of an argument/group name, problems: .variable", fixed=TRUE)
  })

  test_engine("multiple error if subject not df", {
    expect_error({
      capture_first_melt_multiple("foobar")
    }, "subject must be a data.frame")
  })

  test_engine("multiple error if no arg named variable", {
    expect_error({
      capture_first_melt_multiple(D2, baz="foobar")
    }, "pattern must define group named column")
  })

  test_engine("multiple error if no matching column names", {
    expect_error({
      capture_first_melt_multiple(D2, column="foobar")
    }, "no column names match regex")
  })

}
