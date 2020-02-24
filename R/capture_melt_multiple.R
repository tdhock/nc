capture_melt_multiple <- structure(function # Capture and melt into multiple columns
### Attempt to match a regex to subject.df column names,
### then melt the matching columns to multiple
### result columns in a tall data table.
### It is for the common case of melting four or more
### columns of different types in a "wide" input data table
### with regular names.
### For melting into a single result column,
### see capture_melt_single.
(subject.df,
### The data.frame with column name subjects.
  ...,
### Pattern/engine passed to capture_first_vec along with
### nomatch.error=FALSE, for matching input column names to
### reshape. There must be a group named "column" -- each unique value
### captured in this group becomes a reshape column name in the
### output. There must also be at least one other group, and the
### output will contain a column for each other group -- see
### examples. Specifying the regex and output column names using this
### syntax can be less repetitive than using data.table::patterns.
  na.rm=FALSE,
### Remove missing values from melted data? (passed to
### data.table::melt.data.table)
  verbose=getOption("datatable.verbose")
### Print verbose output messages? (passed to
### data.table::melt.data.table)
){
  column <- . <- count <- NULL
  ## Above to avoid CRAN NOTE.
  L <- capture_df_names(subject.df, ...)
  if(is.null(L$match.dt$column)){
    stop("pattern must define group named column")
  }
  if(!is.character(L$match.dt$column)){
    stop(
      "column group must be character, ",
      "but conversion function returned ",
      class(L$match.dt$column)[[1]])
  }
  not.col <- names(L$match.dt)[names(L$match.dt) != "column"]
  if(length(not.col)==0){
    stop("need at least one group other than column")
  }
  by.list <- list(
    group=not.col,
    column="column")
  by.result <- list()
  paste.collapse <- function(x.vec)paste(x.vec, collapse=",")
  for(by.name in names(by.list)){
    by.vec <- by.list[[by.name]]
    by.counts <- L$match.dt[!is.na(column), .(
      count=.N
    ), keyby=by.vec]#need keyby so variable.name order consistent later.
    by.problems <- by.counts[count != max(count)]
    if(nrow(by.problems)){
      count.vec <- sprintf(
        "%s=%d",
        apply(by.counts[, by.vec, with=FALSE], 1, paste.collapse),
        by.counts$count)
      stop(
        "need ",
        paste.collapse(by.vec),
        "=same count for each value, but have: ",
        paste(count.vec, collapse=" "),
        "; please change pattern or edit input column names")
    }
    by.result[[by.name]] <- by.counts
  }
  if(nrow(by.result$column)==1){
    stop(
      "need multiple output columns, ",
      "but only one value (",
      by.result$column$column,
      ") captured in column group; ",
      "either provide a different regex ",
      "that captures more than one value in column group, ",
      "or use capture_melt_single ",
      "if you really want only one output column")
  }
  i.name <- paste(names(L$match.dt), collapse="")
  i.dt <- data.table(L$match.dt)
  set(i.dt, j=i.name, value=1:nrow(i.dt))
  ##need to sort by not.col for irregular col ord.
  setkeyv(i.dt, c("column", not.col))
  measure.dt <- i.dt[!is.na(column), list(
    indices=list(.SD[[i.name]])
  ), by=column]
  id.vars <- names(subject.df)[L$no.match]
  stop_for_capture_same_as_id(not.col, id.vars)
  value.name <- measure.dt$column
  out.names <- c(id.vars, not.col, value.name)
  variable.name <- paste(out.names, collapse="")
  check.list <- list(
    "input column names which do not match the pattern"=id.vars,
    "other regex group names"=not.col)
  for(check.name in names(check.list)){
    check.values <- check.list[[check.name]]
    bad.values <- value.name[value.name %in% check.values]
    if(length(bad.values)){
      stop(
        "unable to create unique output column names; ",
        "some values (",
        paste(bad.values, collapse=", "),
        ") captured by the regex group named column ",
        "are the same as ",
        check.name,
        "; please change either the pattern or the ",
        check.name,
        " so that output column names will be unique")
    }
  }
  melted <- melt(
    data.table(subject.df),
    id.vars=which(is.na(L$match.dt$column)),
    measure.vars=measure.dt$indices,
    ##seealso<< Internally we call data.table::melt.data.table with
    ##value.name=a character vector of unique values
    ##of the column capture group, and
    ##measure.vars=a list of corresponding column indices.
    variable.name=variable.name,
    value.name=value.name,
    na.rm=na.rm,
    variable.factor=FALSE,#character for join.
    value.factor=FALSE,
    verbose=verbose)
  ## Join on variable but remove it since we require the user to
  ## provide at least one other group which should be more
  ## informative/interpretable, which makes variable useless.
  set(by.result$group, j=variable.name, value=paste(1:nrow(by.result$group)))
  ## Order of join important below, when "count" is one of the
  ## out.names, so that the data column is selected, rather than the
  ## variable created for error checking when creating by.counts
  ## above.
  melted[by.result$group, out.names, with=FALSE, on=variable.name]
### Data table of melted/tall data, with a new column for each unique
### value of the capture group named "column", and a new column for
### each other capture group.
}, ex=function(){

  ## Example 1: melt iris columns to compare Sepal and Petal dims, as
  ## in cdata package, https://winvector.github.io/cdata/
  (iris.part.cols <- nc::capture_melt_multiple(
    iris,
    column=".*?",
    "[.]",
    dim=".*"))
  iris.part.cols[Sepal<Petal] #Sepals are never smaller than Petals.
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(dim ~ Species)+
      coord_equal()+
      geom_abline(slope=1, intercept=0, color="grey")+
      geom_point(aes(
        Petal, Sepal),
        shape=1,
        data=iris.part.cols)
  }

  ## Example 2. melt iris to Length and Width columns.
  (iris.dim.cols <- nc::capture_melt_multiple(
    iris,
    part=".*?",
    "[.]",
    column=".*"))
  iris.dim.cols[Length<Width] #Length is never less than Width.

  ## Example 3. Lots of column types, from example(melt.data.table).
  set.seed(1)
  DT <- data.table::data.table(
    i_1 = c(1:5, NA),
    i_2 = c(NA,6:10),
    f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
    f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
    c_1 = sample(c(letters[1:3], NA), 6, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
  ## nc syntax melts to three output columns of different types using
  ## a single regex (na.rm=FALSE by default in order to avoid losing
  ## information).
  nc::capture_melt_multiple(
    DT,
    column="[^c]",
    "_",
    number="[12]")

  ## Example 4, three children, one family per row, from data.table
  ## vignette.
  family.dt <- data.table::fread(text="
family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
  ## nc::field can be used to define group name and pattern at the
  ## same time, to avoid repetitive code.
  (children.nc <- nc::capture_melt_multiple(
    family.dt,
    column=".+",
    "_",
    nc::field("child", "", "[1-3]"),
    na.rm=TRUE))

})


