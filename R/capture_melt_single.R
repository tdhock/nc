capture_melt_single <- structure(function # Capture and melt into a single column
### Attempt to match a regex to subject.df column names,
### then melt the matching columns to a single
### result column in a tall data table,
### and add output columns for each group defined in the regex.
### It is for the common case of melting
### several columns of the same type in a "wide" input data table which has
### several distinct pieces of information encoded in each column
### name. For melting into several result columns of different types, see
### capture_melt_single_multiple.
(subject.df,
### The data.frame with column name subjects.
  ...,
### Pattern/engine passed to capture_first_vec along with
### nomatch.error=FALSE, for matching input column names.
  id.vars=NULL,
### Columns to copy to the output data table (passed to
### data.table::melt.data.table). Default NULL means to use all
### columns not matched by the pattern.
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
  ##arguments. In contrast capture_melt_single uses the specified
  ##pattern for both purposes, which avoids some repetition in user
  ##code.
  if(!is.data.frame(subject.df)){
    stop("subject must be a data.frame")
  }
  ##details<< capture_first_vec is called to perform regex matching on
  ##the input column names.
  match.dt <- capture_first_vec(
    names(subject.df),
    ...,
    nomatch.error=FALSE)
  no.match <- apply(is.na(match.dt), 1, all)
  if(all(no.match)){
    stop(
      "no column names match regex below\n",
      var_args_list(...)$pattern)
  }
  names.dt.args <- list(match.dt)
  variable.name <- paste(c(names(subject.df), names(match.dt)), collapse=".")
  names.dt.args[[variable.name]] <- names(subject.df)
  names.dt <- do.call(data.table, names.dt.args)[!no.match]
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
  id.names <- if(is.integer(id.vars))names(subject.df)[id.vars] else id.vars
  ##details<< as in data.table::melt.data.table, the order of the
  ##output columns is id.vars (columns copied from input), columns
  ##captured from variable names, value column.
  out.names <- c(id.names, names(match.dt), value.name)
  tall.dt[names.dt, out.names, with=FALSE, on=variable.name]
### Data table of melted/tall data, with a new column for each named
### argument in the pattern, and additionally variable/value columns.
}, ex=function(){

  ## Example 1: melt and then dcast iris data, as in cdata package,
  ## https://winvector.github.io/cdata/
  (iris.tall <- nc::capture_melt_single(
    iris,
    part=".*",
    "[.]",
    dim=".*",
    value.name="cm"))
  ## Histogram of cm for each variable.
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(part ~ dim)+
      geom_bar(aes(cm), data=iris.tall)
  }

  ## Example 2: melt who data and use type conversion functions for
  ## year limits (e.g. for censored regression).
  if(requireNamespace("tidyr")){
    data(who, package="tidyr", envir=environment())
    ##2.1 just extract diagnosis and gender to chr columns.
    new.diag.gender <- list(#save pattern as list for re-use later.
      "new_?",
      diagnosis=".*",
      "_",
      gender=".")
    who.tall.chr <- nc::capture_melt_single(who, new.diag.gender, na.rm=TRUE)
    print(head(who.tall.chr))
    str(who.tall.chr)
    ##2.2 also extract ages and convert to numeric output columns.
    who.tall.num <- nc::capture_melt_single(
      who,
      new.diag.gender,#previously pattern for matching diagnosis and gender.
      ages=list(#new pattern for matching age range.
        min.years="0|[0-9]{2}", as.numeric,#in-line type conversion functions.
        max.years="[0-9]{0,2}", function(x)ifelse(x=="", Inf, as.numeric(x))),
      value.name="count",
      id.vars=c("iso2", "year"),
      na.rm=TRUE)
    print(head(who.tall.num))
    str(who.tall.num)
    ##2.3 compute total count for each age range then display the
    ##subset with max.years lower than a threshold.
    who.age.counts <- who.tall.num[, .(
      total=sum(count)
    ), by=.(min.years, max.years)]
    print(who.age.counts[max.years < 50])
  }

})

