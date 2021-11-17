capture_first_df <- structure(function # Capture first match in columns of a data frame
### Capture first matching text from one or more character columns of
### a data frame, using a different regular expression for each
### column.
(# can NOT have subject arg outside of dots (column named subject)
  ...,
### subject data frame, colName1=list(groupName1=pattern1, fun1, etc),
### colName2=list(etc), etc. First argument must be a data frame with
### one or more character columns of subjects for matching. If the
### first argument is a data table then it will be modified using
### data.table::set (for memory efficiency, to avoid copying the whole
### data table); otherwise the input data frame will be copied to a
### new data table. Each other argument must be named using a column
### name of the subject data frame, e.g. colName1, colName2. Each
### other argument value must be a list that specifies the
### regex/conversion to use (in string/function/list format as
### documented in capture_first_vec, which is used on each named
### column), and possibly a column-specific engine to use.
  nomatch.error=getOption("nc.nomatch.error", TRUE),
### if TRUE (default), stop with an error if any subject does not
### match; otherwise subjects that do not match are reported as
### missing/NA rows of the result.
  existing.error=getOption("nc.existing.error", TRUE),
### if TRUE (default to avoid data loss), stop with an error if any
### capture groups have the same name as an existing column of
### subject.
  engine=getOption("nc.engine", "PCRE")
### character string, one of PCRE, ICU, RE2. This engine will be used
### for each column, unless another engine is specified for that
### column in ...
){
  all.arg.list <- list(...)
  subject <- all.arg.list[[1]]
  if(!is.data.table(subject)){
    if(!is.data.frame(subject)){
      stop("subject must be a data.frame with character columns to match")
    }
    subject <- as.data.table(subject)
  }
  col.pattern.list <- all.arg.list[-1]
  if(length(col.pattern.list)==0){
    stop(domain=NA, gettext("no patterns specified in ...; must specify subjectColName=list(groupName=pattern, etc), etc"))
  }
  valid.name <- names(col.pattern.list) %in% names(subject)
  invalid.vec <- names(col.pattern.list)[!valid.name]
  if(is.null(names(col.pattern.list)) || length(invalid.vec)){
    stop(domain=NA, gettextf("named args (%s) not found in subject column names (%s); each pattern in ... must be named using a column name of subject", paste(invalid.vec, collapse = ", "), paste(names(subject), collapse = ", ")))
  }
  if(names(all.arg.list)[[1]] != ""){
    stop("first argument (subject data.frame) should not be named")
  }
  name.tab <- table(names(col.pattern.list))
  if(any(bad <- 1 < name.tab)){
    stop(domain=NA, gettextf("each argument / subject column name should be unique, problems: %s", paste(names(name.tab)[bad], collapse = ", ")))
  }
  name.group.used <- FALSE
  for(col.name in names(col.pattern.list)){
    subject.vec <- subject[[col.name]]
    col.arg.list <- c(list(subject.vec), col.pattern.list[[col.name]])
    maybe.rep <- c("engine", "nomatch.error")
    to.rep <- maybe.rep[!maybe.rep %in% names(col.arg.list)]
    col.arg.list[to.rep] <- lapply(to.rep, get, environment())
    tryCatch({
      m <- do.call(capture_first_vec, col.arg.list)
    }, error=function(e){
      stop(domain=NA, gettextf("problem for subject column %s: %s", col.name, e))
    })
    new.bad <- names(m) %in% names(subject)
    if(isTRUE(existing.error) && any(new.bad)){
      stop(domain=NA, gettextf("capture group names (%s) must not conflict with existing column names (%s); fix by changing capture group names or use existing.error=FALSE to overwrite existing column names", paste(names(m), collapse = ", "), paste(names(subject), collapse = ", ")))
    }
    set(subject, j=names(m), value=m)
  }
  subject
### data.table with same number of rows as subject, with an additional
### column for each named capture group specified in ...
}, ex=function(){

  ## The JobID column can be match with a complicated regular
  ## expression, that we will build up from small sub-pattern list
  ## variables that are easy to understand independently.
  (sacct.df <- data.frame(
    JobID = c(
      "13937810_25", "13937810_25.batch",
      "13937810_25.extern", "14022192_[1-3]", "14022204_[4]"),
    Elapsed = c(
      "07:04:42", "07:04:42", "07:04:49",
      "00:00:00", "00:00:00"),
    stringsAsFactors=FALSE))

  ## Just match the end of the range.
  int.pattern <- list("[0-9]+", as.integer)
  end.pattern <- list(
    "-",
    task.end=int.pattern)
  nc::capture_first_df(sacct.df, JobID=list(
    end.pattern, nomatch.error=FALSE))

  ## Match the whole range inside square brackets.
  range.pattern <- list(
    "[[]",
    task.start=int.pattern,
    end.pattern, "?", #end is optional.
    "[]]")
  nc::capture_first_df(sacct.df, JobID=list(
    range.pattern, nomatch.error=FALSE))

  ## Match either a single task ID or a range, after an underscore.
  task.pattern <- list(
    "_",
    list(
      task.id=int.pattern,
      "|",#either one task(above) or range(below)
      range.pattern))
  nc::capture_first_df(sacct.df, JobID=task.pattern)

  ## Match type suffix alone.
  type.pattern <- list(
    "[.]",
    type=".*")
  nc::capture_first_df(sacct.df, JobID=list(
    type.pattern, nomatch.error=FALSE))

  ## Match task and optional type suffix.
  task.type.pattern <- list(
    task.pattern,
    type.pattern, "?")
  nc::capture_first_df(sacct.df, JobID=task.type.pattern)

  ## Match full JobID and Elapsed columns.
  nc::capture_first_df(
    sacct.df,
    JobID=list(
      job=int.pattern,
      task.type.pattern),
    Elapsed=list(
      hours=int.pattern,
      ":",
      minutes=int.pattern,
      ":",
      seconds=int.pattern))

  ## If input is data table then it is modified for memory efficiency,
  ## to avoid copying entire table.
  sacct.DT <- data.table::as.data.table(sacct.df)
  nc::capture_first_df(sacct.df, JobID=task.pattern)
  sacct.df #not modified.
  nc::capture_first_df(sacct.DT, JobID=task.pattern)
  sacct.DT #modified!

})
