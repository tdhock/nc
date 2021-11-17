### Check that first argument is a data frame and then call
### check_names on its names.
check_df_names <- function(...){
  all.args <- list(...)
  subject.df <- all.args[[1]]
  var.args <- all.args[-1]
  if(!is.data.frame(subject.df)){
    stop("first argument (subject) must be a data.frame")
  }
  subject.vec <- names(subject.df)
  ans <- check_names(subject.vec, var.args)
  ans$data <- if(is.data.table(subject.df)){
    subject.df
  } else {
    data.table(subject.df)
  }
  ans
}

### Check that subject is a vector of unique names and then call
### capture_first_vec.
check_names <- function(subject, var.args){
  names.tab <- table(subject)
  names.rep <- names.tab[1 < names.tab]
  if(length(names.rep)){
    stop(domain=NA, gettextf("input must have columns with unique names, problems: %s", paste(names(names.rep), collapse = ", ")))
  }
  capture.args <- function(L){
    c(list(subject), L, nomatch.error=FALSE)
  }
  match.dt <- do.call(capture_first_vec, capture.args(var.args))
  missing.vec <- apply(is.na(match.dt), 1, all)
  if(all(missing.vec)){
    no.funs.args <- Filter(function(x)!is.function(x), unlist(var.args))
    no.funs.dt <- do.call(capture_first_vec, capture.args(no.funs.args))
    no.funs.missing <- apply(is.na(no.funs.dt), 1, all)
    if(all(no.funs.missing)){
      stop(domain=NA, gettextf("no column names match regex below, please change regex or column names
%s", var_args_list(var.args)[["pattern"]]))
    }else{
      no.funs.names <- subject[which(!no.funs.missing)]
      last <- length(no.funs.names)
      disp.N <- 10
      disp.names <- if(last <= disp.N*2){
        no.funs.names
      }else{
        c(no.funs.names[1:disp.N], "...", no.funs.names[(last-disp.N):last])
      }
      stop(domain=NA, gettextf("need to change type conversion function(s), which should return at least one non-NA, but are always returning NA, even though regex matched %s column(s): %s", last, paste(disp.names, collapse = ",")))
    }
  }
  list(
    match.dt=match.dt,
    no.match=missing.vec)
}
