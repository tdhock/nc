capture_first_melt <- structure(function # Capture column names and melt
### Match a regex to subject.df column names,
### then melt the matching columns to a single
### result column in a tall data table,
### and add output columns for each group defined in the regex.
### It is for the common case of melting
### several columns of the same type in a "wide" input data table which has
### several distinct pieces of information encoded in each column
### name. For melting into several result columns of different types, see
### capture_first_melt_multiple.
(subject.df,
### The data.frame with column name subjects.
  ...,
### Pattern passed to capture_first_vec.
  id.vars=NULL,
### Columns to copy to the output data table (passed to
### data.table::melt.data.table). Default NULL means to use all
### columns not matched by the pattern.
  variable.name="variable",
### Name of the column in output which has values taken from melted
### column names of input (passed to data.table::melt.data.table).
  value.name="value",
### Name of the column in output which has values taken from melted
### column values of input (passed to data.table::melt.data.table).
  na.rm=FALSE,
### remove missing values from melted data? (passed to
### data.table::melt.data.table)
  verbose=getOption("datatable.verbose")
### Print verbose output messages? (passed to
### data.table::melt.data.table)
){
  ##seealso<< This function is inspired by tidyr::pivot_longer which
  ##requires some repetition, i.e. the columns to melt and pattern to
  ##match the melted column names must be specified in separate
  ##arguments. In contrast capture_first_melt uses the specified
  ##pattern for both purposes, which avoids some repetition in user
  ##code.
  if(!is.data.frame(subject.df)){
    stop("subject must be a data.frame")
  }
  variable <- names(subject.df)
  ##details<< capture_first_vec is called to perform regex matching on
  ##the input column names.
  match.dt <- capture_first_vec(
    variable,
    ...,
    nomatch.error=FALSE)
  no.match <- apply(is.na(match.dt), 1, all)
  if(all(no.match)){
    stop(
      "no column names match regex below\n",
      var_args_list(...)$pattern)
  }
  names.dt <- data.table(variable, match.dt)[!no.match]
  if(is.null(id.vars)){
    id.vars <- which(no.match)
  }
  ##details<< data.table::melt.data.table is called to perform the
  ##melt operation.
  tall.dt <- melt(
    data.table(subject.df),
    id.vars=id.vars,
    measure.vars=which(!no.match),
    variable.name=variable.name,
    value.name=value.name,
    na.rm=na.rm,
    variable.factor=FALSE, #character columns are preferred in joins.
    value.factor=FALSE,
    verbose=verbose)
  on.vec <- structure("variable", names=variable.name)
  tall.dt[names.dt, on=on.vec]
### Data table of melted/tall data, with a new column for each named
### argument in the pattern, and additionally variable/value columns.
}, ex=function(){

  ## Example 1: melt and then dcast iris data, as in cdata package,
  ## https://winvector.github.io/cdata/
  library(data.table)
  iris.dt <- data.table(observation=1:nrow(iris), iris)
  (iris.tall <- nc::capture_first_melt(
    iris.dt,
    part=".*",
    "[.]",
    dim=".*"))

  ## Are sepals bigger than petals? YES.
  (iris.part.cols <- dcast(
    iris.tall,
    observation + Species + dim ~ part))
  iris.part.cols[Sepal<Petal]

  ## Are the flower longer or wider? LONGER (by definition...)
  (iris.dim.cols <- dcast(
    iris.tall,
    observation + Species + part ~ dim))
  iris.dim.cols[Length < Width]

})

