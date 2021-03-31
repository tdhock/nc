capture_first_vec <- structure(function # Capture first match in each character vector element
### Use a regular expression (regex) with capture groups to extract
### the first matching text from each of several subject strings. For
### all matches in one multi-line text file or string use
### capture_all_str. For the first match in every row of a data.frame,
### using a different regex for each column, use capture_first_df. For
### matching column names in a wide data frame and then
### melting/reshaping those columns to a taller/longer data frame, see
### capture_melt_single and capture_melt_multiple. To simplify the
### definition of the regex you can use field, quantifier, and
### alternatives.
(...,
### subject, name1=pattern1, fun1, etc. The first argument
### must be a character vector of length>0 (subject strings to parse
### with a regex). Arguments after the first specify the
### regex/conversion and must be character/function/list. All
### character strings are pasted together to obtain the final regex
### used for matching. Each string with a named argument in R becomes
### a capture group in the regex, and the name is used for the
### corresponding column of the output data table. Each named pattern
### may be followed by at most one function which is used to convert
### the values captured by that pattern. Lists are parsed recursively
### for convenience.
  nomatch.error=getOption("nc.nomatch.error", TRUE),
### if TRUE (default), stop with an error if any subject does not
### match; otherwise subjects that do not match are reported as
### missing/NA rows of the result.
  engine=getOption("nc.engine", "PCRE")
### character string, one of PCRE, ICU, RE2
){
  L <- subject_var_args(...)
  subject.vec <- L[["subject"]]
  stop_for_engine(engine)
  ##alias<< nc
  stop_for_na <- function(no.match){
    if(isTRUE(nomatch.error) && any(no.match)){
      i <- which(no.match)
      stop(
        "subject",
        ifelse(length(i)==1, "", "s"),
        " ",
        paste(i, collapse=","),
        " did not match regex below; ",
        "to output missing rows use nomatch.error=FALSE\n",
        L[["pattern"]])
    }
  }
  m <- if(engine=="PCRE"){
    vec.with.attrs <- try_or_stop_print_pattern({
      regexpr(L[["pattern"]], subject.vec, perl=TRUE)
    }, L[["pattern"]], engine)
    make.na <- vec.with.attrs == -1 | is.na(subject.vec)
    stop_for_na(make.na)
    first <- attr(vec.with.attrs, "capture.start")
    first[make.na] <- NA
    last <- attr(vec.with.attrs, "capture.length")-1+first
    last[make.na] <- NA
    subs <- substring(subject.vec, first, last)
    matrix(subs, length(subject.vec))
  }else{
    match.fun <- if(engine=="ICU"){
      stringi::stri_match_first_regex
    }else{
      re2r::re2_match
    }
    match.mat <- try_or_stop_print_pattern({
      match.fun(subject.vec, L[["pattern"]])
    }, L[["pattern"]], engine)
    only_captures(match.mat, stop_for_na)
  }
  if(length(L[["fun.list"]]) < ncol(m)){
    stop(
      "regex contains more groups than names; ",
      "please remove literal groups (parentheses) ",
      "from the regex pattern, ",
      "and use named arguments in R code instead")
  }
  apply_type_funs(m, L[["fun.list"]])
### data.table with one row for each subject, and one column for each
### capture group.
}, ex=function(){

  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000",
    "chr1:110-111 chr2:220-222") # two possible matches.
  ## Find the first match in each element of the subject character
  ## vector. Named argument values are used to create capture groups
  ## in the generated regex, and argument names become column names in
  ## the result.
  (dt.chr.cols <- nc::capture_first_vec(
    chr.pos.vec,
    chrom="chr.*?",
    ":",
    chromStart="[0-9,]+"))

  ## Even when no type conversion functions are specified, the result
  ## is always a data.table:
  str(dt.chr.cols)

  ## Conversion functions are used to convert the previously named
  ## group, and patterns may be saved in lists for re-use.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  int.pattern <- list("[0-9,]+", keep.digits)
  range.pattern <- list(
    chrom="chr.*?",
    ":",
    chromStart=int.pattern,
    list( # un-named list becomes non-capturing group.
      "-",
      chromEnd=int.pattern
    ), "?") # chromEnd is optional.
  (dt.int.cols <- nc::capture_first_vec(
    chr.pos.vec, range.pattern))

  ## Conversion functions used to create non-char columns.
  str(dt.int.cols)

  ## NA used to indicate no match or missing subject.
  na.vec <- c(
    "this will not match",
    NA, # neither will this.
    chr.pos.vec)
  nc::capture_first_vec(na.vec, range.pattern, nomatch.error=FALSE)

})

