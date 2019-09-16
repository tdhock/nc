### Error if subject.vec or pattern incorrect type.
stop_for_subject <- function(subject.vec, pattern){
  if(!(
    is.character(subject.vec) &&
    0 < length(subject.vec)
  )){
    stop("subject.vec should be a character vector with length>0")
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
    stop(
      engine,
      " engine not available; try install.packages('",
      pkg, "')")
  }
### character string.
}
