capture_first_melt <- structure(function # Capture column names and melt
### Extract first match of a regex from data.frame column names, by
### calling capture_first_vec on column names, then using the column
### names that matched as measure.vars in data.table::melt.data.table,
### then joining the two results. It is for the common case of melting
### a "wide" data table which has several distinct pieces of
### information encoded in each column name.
(subject.df,
### The data.frame with column name subjects.
  ...,
### Pattern passed to capture_first_vec.
  variable.name="variable",
### Name of the column in output which has values taken from melted
### column names of input (passed to data.table::melt.data.table).
  value.name="value"
### Name of the column in output which has values taken from melted
### column values of input (passed to data.table::melt.data.table).
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
  names.dt <- data.table(variable, match.dt)[!no.match]
  tall.dt <- melt(
    data.table(subject.df),
    id.vars=which(no.match),
    variable.factor=FALSE,
    variable.name=variable.name,
    value.name=value.name)
  on.vec <- structure("variable", names=variable.name)
  tall.dt[names.dt, on=on.vec]
### Data table of melted/tall data, with a new column for each named
### argument in the pattern, and additionally variable/value columns.
}, ex=function(){
  ## who data example inspired from Hadley's talk
  ## https://www.youtube.com/watch?v=qFRYnKdLz5U
  if(requireNamespace("tidyr")){
    data(who, package="tidyr", envir=environment())
    (who.tidy <- nc::capture_first_melt(
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
      variable.name="column"))
    (not.na <- who.tidy[!is.na(count)])
    not.na[, table(diagnosis, gender)]
    not.na[, .(count=.N), by=.(ages, min.years, max.years)]
  }
})
