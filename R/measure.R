measure <- structure(function
### Computes a value to be used as measure.vars argument to
### data.table::melt.data.table.
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

### Compute a list of arguments to pass to
### data.table::melt.data.table.
melt_list <- function(measure.fun, dot.args, ...){
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

### Compute a measure.vars vector (indicating a single output column)
### with variable_table attribute to pass to
### data.table::melt.data.table.
measure_single <- function(subject.names, match.dt, no.match, value.name=NULL){
  id.vars <- subject.names[no.match]
  stop_for_capture_same_as_id(names(match.dt), id.vars)
  if(!is.null(value.name)){
    check.list <- list(
      "an input column name that did not match the pattern"=subject.names,
      "a capture group name"=names(match.dt))
    for(check.name in names(check.list)){
      check.values <- check.list[[check.name]]
      if(value.name %in% check.values){
        stop(
          "value.name (",
          value.name,
          ") is the same as ",
          check.name,
          "; please change one ",
          "so that all output column names will be unique")
      }
    }
  }
  structure(
    which(!no.match),
    variable_table=match.dt[!no.match])
}

### Compute a measure.vars list (indicating multiple output columns)
### with variable_table attribute to pass to
### data.table::melt.data.table.
measure_multiple <- function(subject.names, match.dt, no.match, fill=TRUE){
  column <- . <- NULL
  ## Above to avoid CRAN NOTE.
  if(is.null(match.dt[["column"]])){
    stop("pattern must define group named column")
  }
  if(!is.character(match.dt[["column"]])){
    stop(
      "column group must be character, ",
      "but conversion function returned ",
      class(match.dt[["column"]])[[1]])
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
      stop(
        "need ",
        paste.collapse(by.vec),
        "=same count for each value, but have: ",
        paste(count.vec, collapse=" "),
        "; please change pattern, ",
        "edit input column names, ",
        "or use fill=TRUE to output missing values")
    }
    by.result[[by.name]] <- by.counts
  }
  by.column <- by.result[["column"]]
  if(all(by.column[[i.name]] == 1)){
    stop(
      "only one input variable for each value captured in column group; ",
      "typically this happens when the column group ",
      "matches the entire input column name; ",
      "fix by changing regex so that column group ",
      "matches a strict substring (not the entire input column names)")
  }
  if(nrow(by.column)==1){
    stop(
      "need multiple output columns, ",
      "but only one value (",
      by.column[["column"]],
      ") captured in column group; ",
      "either provide a different regex ",
      "that captures more than one value in column group, ",
      "or use capture_melt_single ",
      "if you really want only one output column")
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
  all.dt <- data.table(do.call(expand.grid, all.list))
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
