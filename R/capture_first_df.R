capture_first_df <- structure(function # Capture first match in columns of a data.frame
### Extract text from several columns of a data.frame, using a
### different regular expression for each column. Uses
### capture_first_vec on each column/pattern indicated in
### ... -- argument names are interpreted as column names of subject;
### argument values are passed as the pattern to
### capture_first_vec.
(# can NOT have subject arg outside of dots (column named subject)
  ...,
### subject.df, colName1=list(groupName1=pattern1, fun1, etc),
### colName2=list(etc), etc. First (un-named) argument should be a
### data.frame with character columns of subjects for matching. The
### other arguments need to be named (and the names e.g. colName1 and
### colName2 need to be column names of the subject data.frame). The
### other argument values specify the regular expression, and must be
### character/function/list. All patterns must be character vectors of
### length 1. If the pattern is a named argument in R,
### it becomes a capture group in the regex. All
### patterns are pasted together to obtain the final pattern used for
### matching. Each named pattern may be followed by at most one
### function (e.g. fun1) which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
  nomatch.error=getOption("nc.nomatch.error", TRUE),
### if TRUE (default), stop with an error if any subject does not
### match; otherwise subjects that do not match are reported as
### missing/NA rows of the result.
  engine=getOption("nc.engine", "PCRE")
### character string, one of PCRE, ICU, RE2
){
  all.arg.list <- list(...)
  subject <- all.arg.list[[1]]
  if(!is.data.frame(subject)){
    stop("subject must be a data.frame with character columns to match")
  }
  col.pattern.list <- all.arg.list[-1]
  if(length(col.pattern.list)==0){
    stop(
      "no patterns specified in ...; ",
      "must specify subjectColName=list(groupName=pattern, etc), etc")
  }
  valid.name <- names(col.pattern.list) %in% names(subject)
  invalid.vec <- names(col.pattern.list)[!valid.name]
  if(is.null(names(col.pattern.list)) || length(invalid.vec)){
    stop("named args (", paste(invalid.vec, collapse=", "),
         ") not found in subject column names (", paste(names(subject), collapse=", "),
         "); each pattern in ... must be named using a column name of subject")
  }
  if(names(all.arg.list)[[1]] != ""){
    stop("first argument (subject data.frame) should not be named")
  }
  name.tab <- table(names(col.pattern.list))
  if(any(bad <- 1 < name.tab)){
    stop(
      "each argument / subject column name should be unique, problems: ",
      paste(names(name.tab)[bad], collapse=", "))
  }
  out <- data.table(subject)
  name.group.used <- FALSE
  for(col.name in names(col.pattern.list)){
    subject.vec <- out[[col.name]]
    col.arg.list <- c(list(subject.vec), col.pattern.list[[col.name]])
    maybe.rep <- c("engine", "nomatch.error")
    to.rep <- maybe.rep[!maybe.rep %in% names(col.arg.list)]
    col.arg.list[to.rep] <- lapply(to.rep, get, environment())
    tryCatch({
      m <- do.call(capture_first_vec, col.arg.list)
    }, error=function(e){
      stop("problem for subject column ", col.name, ": ", e)
    })
    new.bad <- names(m) %in% names(out)
    if(any(new.bad)){
      stop(
        "capture group names (",
        paste(names(m), collapse=", "),
        ") must not conflict with existing column names (",
        paste(names(out), collapse=", "),
        ")")
    }
    out <- cbind(out, m, stringsAsFactors=FALSE)
  }
  out
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
  (task.df <- nc::capture_first_df(
    sacct.df,
    JobID=list(
      job=int.pattern,
      task.type.pattern),
    Elapsed=list(
      hours=int.pattern,
      ":",
      minutes=int.pattern,
      ":",
      seconds=int.pattern)))
  str(task.df)

})
