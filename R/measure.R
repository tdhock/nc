measure <- structure(function
### Computes a value to be used as measure.vars argument to
### data.table::melt.data.table. NOTE: only works on newer versions of
### data.table that include the measure function.
(...,
### Regular expression pattern list, passed to capture_first_vec with
### cols as subject.
 cols
### Character vector, column names to match with regex.
){
  var.args <- list(...)
  match.info <- check_names(cols, var.args)
  match.info[["subject.names"]] <- cols
  ##details<<
  ##measure_multiple is called if there is a capture group
  ##named "column" and measure_single is called otherwise.
  m.fun <- if("column" %in% names(match.info[["match.dt"]])){
    measure_multiple
  }else{
    measure_single
  }
  do.call(m.fun, match.info)
### List or vector to use as measure.vars argument to
### data.table::melt.data.table.
}, ex=function(){

  library(data.table)
  iris.dt <- data.table(datasets::iris[c(1,150),])
  melt(iris.dt, measure=nc::measure(part  =".*", "[.]", dim   =".*"))
  melt(iris.dt, measure=nc::measure(column=".*", "[.]", dim   =".*"))
  melt(iris.dt, measure=nc::measure(part  =".*", "[.]", column=".*"))

})

melt_list <- function
### Compute a list of arguments to pass to
### data.table::melt.data.table.
(measure.fun,
### measure_single or measure_multiple.
  dot.args,
### list of arguments for check_df_names.
  ...
### passed to measure.fun.
){
  match.info <- do.call(check_df_names, dot.args)
  data.i <- which(names(match.info) == "data")
  subject.dt <- match.info[[data.i]]
  measure.args <- c(
    list(..., subject.names=names(subject.dt)),
    match.info[-data.i])
  measure.vars <- do.call(measure.fun, measure.args)
  list(
    data=subject.dt,
    measure.vars=measure.vars)
}

measure_single <- function
### Compute a measure.vars vector (indicating a single output column)
### with variable_table attribute to pass to
### data.table::melt.data.table.
(subject.names,
### character vector of data frame column names.
  match.dt,
### data table of matches.
  no.match,
### logical vector.
  value.name=NULL
### string.
){
  id.vars <- subject.names[no.match]
  stop_for_capture_same_as_id(names(match.dt), id.vars)
  if(!is.null(value.name)){
    check.list <- list(
      "an input column name that did not match the pattern"=subject.names,
      "a capture group name"=names(match.dt))
    for(check.name in names(check.list)){
      check.values <- check.list[[check.name]]
      if(value.name %in% check.values){
        stop(domain=NA, gettextf("value.name (%s) is the same as %s; please change one so that all output column names will be unique", value.name, check.name))
      }
    }
  }
  structure(
    which(!no.match),
    variable_table=match.dt[!no.match])
}

measure_multiple <- function
### Compute a measure.vars list (indicating multiple output columns)
### with variable_table attribute to pass to
### data.table::melt.data.table.
(subject.names,
### character vector of data frame column names.
  match.dt,
### data table of matches.
  no.match,
### logical vector.
  fill=TRUE
### logical.
){
  column <- . <- NULL
  ## Above to avoid CRAN NOTE.
  if(is.null(match.dt[["column"]])){
    stop("pattern must define group named column")
  }
  if(!is.character(match.dt[["column"]])){
    stop(domain=NA, gettextf("column group must be character, but conversion function returned %s", class(match.dt[["column"]])[[1]]))
  }
  not.col <- names(match.dt)[names(match.dt) != "column"]
  if(length(not.col)==0){
    stop("need at least one group other than column")
  }
  id.vars <- subject.names[no.match]
  stop_for_capture_same_as_id(not.col, id.vars)
  by.list <- list(
    group=not.col,
    column="column")
  by.result <- list()
  i.name <- paste(names(match.dt), collapse="")
  paste.collapse <- function(x.vec)paste(x.vec, collapse=",")
  for(by.name in names(by.list)){
    by.vec <- by.list[[by.name]]
    by.counts <- match.dt[!is.na(column), {
      structure(list(.N), names=i.name)
    }, keyby=by.vec]#need keyby so variable.name order consistent later.
    count <- by.counts[[i.name]]
    if(!isTRUE(fill) && any(count != max(count))){
      count.vec <- sprintf(
        "%s=%d",
        apply(by.counts[, by.vec, with=FALSE], 1, paste.collapse),
        by.counts[[i.name]])
      stop(domain=NA, gettextf("need %s=same count for each value, but have: %s; please change pattern, edit input column names, or use fill=TRUE to output missing values", paste.collapse(by.vec), paste(count.vec, collapse = " ")))
    }
    by.result[[by.name]] <- by.counts
  }
  by.column <- by.result[["column"]]
  if(all(by.column[[i.name]] == 1)){
    stop(domain=NA, gettext("only one input variable for each value captured in column group; typically this happens when the column group matches the entire input column name; fix by changing regex so that column group matches a strict substring (not the entire input column names)"))
  }
  if(nrow(by.column)==1){
    stop(domain=NA, gettextf("need multiple output columns, but only one value (%s) captured in column group; either provide a different regex that captures more than one value in column group, or use capture_melt_single if you really want only one output column", by.column[["column"]]))
  }
  i.name <- paste(names(match.dt), collapse="")
  i.dt <- data.table(match.dt)
  set(i.dt, j=i.name, value=1:nrow(i.dt))
  ##need to sort by not.col for irregular col ord.
  all.list <- lapply(match.dt, function(x)sort(unique(x[!is.na(x)])))
  value.name <- all.list[["column"]]
  check.list <- list(
    "input column names which do not match the pattern"=id.vars,
    "other regex group names"=not.col)
  for(check.name in names(check.list)){
    check.values <- check.list[[check.name]]
    bad.values <- value.name[value.name %in% check.values]
    if(length(bad.values)){
      stop(domain=NA, gettextf("unable to create unique output column names; some values (%s) captured by the regex group named column are the same as %s; please change either the pattern or the %s so that output column names will be unique", paste(bad.values, collapse = ", "), check.name, check.name))
    }
  }
  all.dt <- do.call(CJ, all.list)
  i.all.dt <- i.dt[all.dt, on=names(all.dt)]
  setkeyv(i.all.dt, c("column", not.col))
  var.tab <- by.result[["group"]][, not.col, with=FALSE]
  measure.vars <- structure(
    list(), variable_table=var.tab)
  for(col.value in value.name){
    measure.vars[[col.value]] <- i.all.dt[col.value, .SD[[i.name]] ]
  }
  measure.vars
}
