capture_first_melt <- structure(function # Capture column names and melt
### Extract first match of a regex from data.frame column names, by
### calling capture_first_vec on column names, then using the column
### names that matched as measure.vars in data.table::melt.data.table,
### then joining the two results. It is for the common case of melting
### several columns of the same type in a "wide" data table which has
### several distinct pieces of information encoded in each column
### name. For melting columns of different types, see
### capture_first_melt_multiple.
(subject.df,
### The data.frame with column name subjects.
  ...,
### Pattern passed to capture_first_vec.
  id.vars=NULL,
### Columns which should not be melted (passed to
### data.table::melt.data.table). Default NULL means to use all
### columns which do not match the specified pattern.
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

  ## Are the flower longer or wider? LONGER (by definition...)
  (iris.dim.cols <- dcast(
    iris.tall,
    observation + Species + part ~ dim))
  iris.dim.cols[Length < Width]
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      Theme(Panel.spacing=grid::unit(0, "lines"))+
      facet_grid(part ~ Species)+
      coord_equal()+
      geom_abline(slope=1, intercept=0, color="grey")+
      geom_point(aes(
        Width, Length),
        data=iris.dim.cols)
  }

  ## using reshape requires a pre-processing step.
  measure.i <- grep("[.]", names(iris))
  iris.cm <- data.frame(iris)
  names(iris.cm)[measure.i] <- paste0("mm_", names(iris)[measure.i])
  reshape(
    iris.cm,
    measure.i,
    direction="long",
    sep="_")

  ## Example 2: WHO data inspired from Hadley's talk
  ## https://www.youtube.com/watch?v=qFRYnKdLz5U
  if(requireNamespace("tidyr")){
    data(who, package="tidyr", envir=environment())
    who.tall <- nc::capture_first_melt(
      who,
      "new_?",
      diagnosis=".*",
      "_",
      gender=".",
      ages=list(
        min.years="0|[0-9]{2}", as.numeric,
        max.years=list("[0-9]{2}"), "?",
        function(x)ifelse(x=="", Inf, as.numeric(x))),
      value.name="count",
      variable.name="column",
      na.rm=TRUE)
    print(who.tall)
    print(who.tall[, table(diagnosis, gender)])
    print(who.tall[, .(count=.N), by=.(ages, min.years, max.years)])
    ## gather works well but does not make a separate output column
    ## for each of the three pieces of info in an input column name.
    tidyr::gather(who, "column", "count", grep("new", names(who)), na.rm=TRUE)
    ## reshape with pre-processing works, but again no separate output
    ## columns.
    is.varying <- grepl("new", names(who))
    who.df <- data.frame(who)
    names(who.df)[is.varying] <- paste0("count.", names(who)[is.varying])
    tall.na.df <- reshape(
      who.df,
      direction="long",
      timevar="variable",
      varying=is.varying)
    head(tall.na.df)
    tall.df <- subset(tall.na.df, !is.na(count))
    head(tall.df)
  }

})

