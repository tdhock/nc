### Error if capture names same as id.vars.
stop_for_capture_same_as_id <- function(capture.vars, id.vars){
  id.captures <- id.vars[id.vars %in% capture.vars]
  if(length(id.captures)){
    stop(
      "some capture group names (",
      paste(id.captures, collapse=", "),
      ") are the same as input column names ",
      "that did not match the pattern; ",
      "please change either the pattern ",
      "or the capture group names ",
      "so that all output column names will be unique")
  }
}

### Error if subject or pattern incorrect type.
stop_for_subject <- function(subject, pattern){
  if(!(
    is.character(subject) &&
    0 < length(subject)
  )){
    stop(
      "subject has class=",
      paste(class(subject), collapse=","),
      " and length=",
      length(subject),
      " but should be a character vector with length>0")
  }
}

stop_for_engine <- function
### Stop if specified engine is not available.
(engine
### character string: PCRE, RE2, or ICU.
){
  pkg.vec <- c(
    PCRE="base",
    RE2="re2r",
    ICU="stringi")
  if(!(
    is.character(engine) &&
    length(engine)==1 &&
    !is.na(engine) &&
    engine %in% names(pkg.vec)
  )){
    stop(
      "engine must be character string, one of: ",
      paste(names(pkg.vec), collapse=", "))
  }
  pkg <- pkg.vec[[engine]]
  if(!requireNamespace(pkg, quietly=TRUE)){
    install.code <- c(
      re2r="remotes::install_github('qinwf/re2r')",
      stringi="install.packages('stringi')")[[pkg]]
    stop(
      engine,
      " engine not available; try ",
      install.code)
  }
### character string.
}
