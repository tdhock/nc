check_df_names <- function
### Check that first argument is a data frame and then call
### check_names on its names.
(...
### data frame, regex pattern args.
){
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

check_names <- function
### Check that subject is a vector of unique names and then call
### capture_first_vec.
(subject,
### character vector, data frame column names.
  var.args
### regex pattern list.
){
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
      stop(domain=NA, gettextf("need to change type conversion function(s), which should return at least one non-NA, but are always returning NA, even though regex matched %s column(s): %s", length(no.funs.names), collapse_some(no.funs.names)))
    }
  }
  list(
    match.dt=match.dt,
    no.match=missing.vec)
}

collapse_some <- function
### Create character string with some or all items.
(all.vec,
### Vector of all items.
  max.first.last=5,
### Max number of items to show.
  collapse=","
### Passed to paste.
){
  all.n <- length(all.vec)
  some.vec <- if(all.n > max.first.last*2) c(
    all.vec[1:max.first.last],
    "...",
    all.vec[seq(all.n-max.first.last+1, all.n)]
  ) else all.vec
  paste(some.vec, collapse=collapse) 
### Character string formed by paste with collapse on some items of
### all.vec (first/last few items used if length is greater than
### max.first.last*2, otherwise all items).
}  
