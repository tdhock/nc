### Check input data frame for unique names and then call
### capture_first_vec.
capture_df_names <- function(...){
  all.args <- list(...)
  subject.df <- all.args[[1]]
  var.args <- all.args[-1]
  if(!is.data.frame(subject.df)){
    stop("first argument (subject) must be a data.frame")
  }
  names.tab <- table(names(subject.df))
  names.rep <- names.tab[1 < names.tab]
  if(length(names.rep)){
    stop(
      "input must have columns with unique names, problems: ",
      paste(names(names.rep), collapse=", "))
  }
  capture.args <- c(
    list(names(subject.df)),
    var.args,
    nomatch.error=FALSE)
  match.dt <- do.call(capture_first_vec, capture.args)
  no.match <- apply(is.na(match.dt), 1, all)
  if(all(no.match)){
    stop(
      "no column names match regex below\n",
      var_args_list(var.args)[["pattern"]])
  }
  list(match.dt=match.dt, no.match=no.match, subject=subject.df)
}
