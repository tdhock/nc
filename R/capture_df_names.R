### Check input data frame for unique names and then call
### capture_first_vec.
capture_df_names <- function(subject.df, ...){
  if(!is.data.frame(subject.df)){
    stop("subject must be a data.frame")
  }
  names.tab <- table(names(subject.df))
  names.rep <- names.tab[1 < names.tab]
  if(length(names.rep)){
    stop(
      "input must have columns with unique names, problems: ",
      paste(names(names.rep), collapse=", "))
  }
  capture_first_vec(
    names(subject.df),
    ...,
    nomatch.error=FALSE)
}
