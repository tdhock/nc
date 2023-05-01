### Error if capture names same as id.vars.
stop_for_capture_same_as_id <- function(capture.vars, id.vars){
  id.captures <- id.vars[id.vars %in% capture.vars]
  if(length(id.captures)){
    stop(domain=NA, gettextf("some capture group names (%s) are the same as input column names that did not match the pattern; please change either the pattern or the capture group names so that all output column names will be unique", paste(id.captures, collapse = ", ")))
  }
}

### Error if subject or pattern incorrect type.
stop_for_subject <- function(subject, pattern){
  if(!(
    is.character(subject) &&
    0 < length(subject)
  )){
    stop(domain=NA, gettextf("subject has class=%s and length=%s but should be a character vector with length>0", paste(class(subject), collapse = ","), length(subject)))
  }
}

stop_for_engine <- function
### Stop if specified engine is not available.
(engine
### character string: PCRE, RE2, or ICU.
){
  pkg.vec <- c(
    PCRE="base",
    RE2="re2",
    ICU="stringi")
  if(!(
    is.character(engine) &&
    length(engine)==1 &&
    !is.na(engine) &&
    engine %in% names(pkg.vec)
  )){
    stop(domain=NA, gettextf("engine must be character string, one of: %s", paste(names(pkg.vec), collapse = ", ")))
  }
  pkg <- pkg.vec[[engine]]
  if(!requireNamespace(pkg, quietly=TRUE)){
    install.code <- sprintf("install.packages('%s')", pkg)
    stop(domain=NA, gettextf("%s engine not available; try %s", engine, install.code))
  }
### character string.
}
