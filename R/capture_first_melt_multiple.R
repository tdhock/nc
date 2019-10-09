capture_first_melt_multiple <- structure(function # Capture and melt multiple columns of different types
### Match a regex to subject.df column names,
### then melt the matching columns to multiple
### result columns in a tall data table.
### It is for the common case of melting several
### columns of different types in a "wide" input data table
### with regular names.
### For melting into a single result column,
### see capture_first_melt.
(subject.df,
### The data.frame with column name subjects.
  ...,
### Pattern passed to capture_first_vec for matching input column
### names. There must be an argument named "column" -- each unique
### value captured in this group becomes a column name in the
### output. There must also be at least one other named argument, and
### the output will contain a column for each other such named
### argument -- see examples. Specifying the regex and output column
### names using this syntax can be less repetitive than using
### data.table::patterns.
  id.vars=NULL,
### columns to use as id.vars in data.table::melt.data.table. Default
### NULL means to use all variables not matched by the pattern.
  na.rm=FALSE,
### remove missing values from melted data? (passed to
### data.table::melt.data.table)
  verbose=getOption("datatable.verbose")
### Print verbose output messages? (passed to
### data.table::melt.data.table)
){
  column <- . <- count <- .col.i <- NULL
  ## Above to avoid CRAN NOTE.
  if(!is.data.frame(subject.df)){
    stop("subject must be a data.frame")
  }
  match.dt <- capture_first_vec(
    names(subject.df),
    ...,
    nomatch.error=FALSE)
  if(is.null(match.dt$column)){
    stop("pattern must define group named column")
  }
  no.match <- apply(is.na(match.dt), 1, all)
  if(all(no.match)){
    stop(
      "no column names match regex below\n",
      var_args_list(...)$pattern)
  }
  not.col <- names(match.dt)[names(match.dt) != "column"]
  if(length(not.col)==0){
    stop("need at least one group other than column")
  }
  dot.names <- grep("^[.]", names(match.dt), value=TRUE)
  if(length(dot.names)){
    stop(
      "dot (.) must not be used ",
      "at the start of an argument/group name, ",
      "problems: ",
      paste(dot.names, collapse=", "))
  }
  by.list <- list(
    group=not.col,
    column="column")
  by.result <- list()
  for(by.name in names(by.list)){
    by.vec <- by.list[[by.name]]
    by.counts <- match.dt[!is.na(column), .(
      count=.N
    ), keyby=by.vec]#need keyby so .variable order consistent later.
    by.max <- max(by.counts$count)
    by.prob <- by.counts[count != by.max]
    if(nrow(by.prob)){
      count.vec <- sprintf(
        "%s=%d",
        apply(by.counts[, by.vec, with=FALSE], 1, paste, collapse=","),
        by.counts$count)
      stop(
        "need same number of values for each ", by.name,
        " but have: ", paste(count.vec, collapse=" "))
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
      "or use capture_first_melt ",
      "if you really want only one output column")
  }
  col.not.matched <- is.na(match.dt$column)
  if(is.null(id.vars)){
    id.vars <- which(col.not.matched)
  }
  col.name.matched <- names(subject.df)[!col.not.matched]
  id.names <- if(is.integer(id.vars)){
    names(subject.df)[id.vars]
  }else if(is.character(id.vars)){
    id.vars
  }else stop("id.vars must be character or integer")
  id.names.matched <- id.names[id.names %in% col.name.matched]
  if(length(id.names.matched)){
    stop(
      "some id.vars (",
      paste(id.names.matched, collapse=", "),
      ") matched the regex below, but should not")
  }
  i.dt <- match.dt[, data.table(
    .col.i=1:.N,
    .SD,
    key=c("column", not.col))]#need to sort by not.col for irregular col ord.
  measure.dt <- i.dt[!is.na(column), list(indices=list(.col.i)), by=column]
  melted <- melt(
    data.table(subject.df),
    id.vars=id.vars,
    measure.vars=measure.dt$indices,
    ##seealso<< Internally we call data.table::melt.data.table with
    ##value.name=a character vector of unique values
    ##of the column capture group, and
    ##measure.vars=a list of corresponding column indices.
    variable.name=".variable",
    value.name=measure.dt$column,
    na.rm=na.rm,
    variable.factor=TRUE,#integer join on integer later.
    value.factor=FALSE,
    verbose=verbose)
  ## Join on variable but remove it since we require the user to
  ## provide at least one other group which should be more
  ## informative/interpretable, which makes variable useless.
  group.var.dt <- by.result$group[, data.table(
    ".variable"=1:.N,
    .SD[, not.col, with=FALSE])]
  join.dt <- group.var.dt[melted, on=".variable"]
  join.dt[, names(join.dt) != ".variable", with=FALSE]
### Data table of melted/tall data, with a new column for each unique
### value of the capture group named "column", and a new column for
### each other capture group.
}, ex=function(){
  
  ## Example 1: melt iris columns to compare Sepal and Petal dims.
  iris.part.cols <- nc::capture_first_melt_multiple(
    iris,
    column=".*?",
    "[.]",
    dim=".*")
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(dim ~ Species)+
      coord_equal()+
      geom_abline(slope=1, intercept=0, color="grey")+
      geom_point(aes(
        Petal, Sepal),
        data=iris.part.cols)
  }
  
  ## Example 2. Lots of column types, from example(melt.data.table).
  DT <- data.table(
    i_1 = c(1:5, NA),
    i_2 = c(NA,6:10),
    f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
    f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
    c_1 = sample(c(letters[1:3], NA), 6, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
  ## nc syntax creates melts to three output columns of different
  ## types using a single regex.
  nc::capture_first_melt_multiple(
    DT,
    column="^[^c]",
    "_",
    number="[12]")
  
  ## Example 3, three children, one family per row, from data.table
  ## vignette.
  family.dt <- fread(text="
family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
  ## nc::field can be used to define group name and pattern at the
  ## same time, to avoid repetitive code.
  (children.nc <- nc::capture_first_melt_multiple(
     family.dt,
     column="[^_]+",
     "_",
     nc::field("child", "", "[1-3]"),
     na.rm=TRUE))
  
})


