capture_first_vec <- structure(function # Capture first match in each character vector element
### Extract the first match of a regex pattern from each
### of several subject strings. This function uses var_args_list
### to analyze the arguments.
### For the first match in every row of a data.frame, using
### a different regex for each column, use capture_first_df. For all
### matches in one multi-line text file use capture_all_str.
(subject.vec,
### The subject character vector.
  ...,
### name1=pattern1, fun1, etc, which creates the regex (pattern1),
### uses fun1 for conversion, and creates column name1 in the
### output. These arguments specify the regular expression
### pattern and must be character/function/list. All patterns must be
### character vectors of length 1. If the pattern is a named argument
### in R, it becomes a capture group in the
### regex. All patterns are pasted together to obtain the final
### pattern used for matching. Each named pattern may be followed by
### at most one function which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
  nomatch.error=getOption("nc.nomatch.error", TRUE),
### if TRUE (default), stop with an error if any subject does not
### match; otherwise subjects that do not match are reported as
### missing/NA rows of the result.
  engine=getOption("nc.engine", "PCRE")
### character string, one of PCRE, ICU, RE2
){
  stop_for_subject(subject.vec)
  stop_for_engine(engine)
  L <- var_args_list(...)
  ##alias<< nc
  if("name" %in% names(L$fun.list) && !isTRUE(nomatch.error)){
    stop("must use nomatch.error=TRUE with name group")
  }
  stop_for_na <- function(no.match){
    if(isTRUE(nomatch.error) && any(no.match)){
      print(unique(subject.vec[no.match]))
      stop("subjects printed above did not match regex below\n", L$pattern)
    }
  }
  m <- if(engine=="PCRE"){
    vec.with.attrs <- try_or_stop_print_pattern({
      regexpr(L$pattern, subject.vec, perl=TRUE)
    }, L$pattern, engine)
    make.na <- vec.with.attrs == -1 | is.na(subject.vec)
    stop_for_na(make.na)
    first <- attr(vec.with.attrs, "capture.start")
    first[make.na] <- NA
    last <- attr(vec.with.attrs, "capture.length")-1+first
    last[make.na] <- NA
    subs <- substring(subject.vec, first, last)
    matrix(subs, length(subject.vec), length(L$fun.list))
  }else{
    match.fun <- if(engine=="ICU"){
      stringi::stri_match_first_regex
    }else{
      re2r::re2_match
    }
    match.mat <- try_or_stop_print_pattern({
      match.fun(subject.vec, L$pattern)
    }, L$pattern, engine)
    only_captures(match.mat, stop_for_na)
  }
  rownames(m) <- names(subject.vec)
  apply_type_funs(m, L$fun.list)
### data.frame with one row for each subject, and one column for each
### capture group. Row names are taken from names of subject.vec or
### the name group.
}, ex=function(){

  named.subject.vec <- c(
    ten="chr10:213,054,000-213,055,000",
    M="chrM:111,000",
    one="chr1:110-111 chr2:220-222") # two possible matches.
  ## Find the first match in each element of the subject character
  ## vector. Named argument values are used to create capture groups
  ## in the generated regex, and argument names become column names in
  ## the result. Since the subject above is named, those names are
  ## used for the rownames of the result.
  (df.chr.cols <- nc::capture_first_vec(
    named.subject.vec,
    chrom="chr.*?",
    ":",
    chromStart="[0-9,]+",
    list( # un-named list becomes non-capturing group.
      "-",
      chromEnd="[0-9,]+"
    ), "?")) # chromEnd is optional.

  ## Even when no type conversion functions are specified, the result
  ## is always a data.frame:
  str(df.chr.cols)

  ## Conversion functions are used to convert the previously named
  ## group, and patterns may be saved in lists for re-use.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  int.pattern <- list("[0-9,]+", keep.digits)
  range.pattern <- list(
    name="chr.*?", # will be used for rownames when subject is un-named.
    ":",
    chromStart=int.pattern,
    list(
      "-",
      chromEnd=int.pattern
    ), "?")
  (df.int.cols <- nc::capture_first_vec(
    named.subject.vec, range.pattern))

  ## Conversion functions used to create non-char columns.
  str(df.int.cols)

  ## Rownames taken from name group because subject has no names.
  nc::capture_first_vec(unname(named.subject.vec), range.pattern)

  ## NA used to indicate no match or missing subject.
  na.vec <- c(
    nomatch="this will not match",
    missing=NA, # neither will this.
    named.subject.vec)
  nc::capture_first_vec(na.vec, range.pattern, nomatch.error=FALSE)

  ## alternate regex engine, but this example with emoji only works
  ## with recent versions of ICU.
  if(requireNamespace("stringi") && stringi::stri_info()$ICU.version >= 59){
    nc::capture_first_vec(
      "foo a\U0001F60E# bar",
      before=".*?",
      emoji="\\p{EMOJI_Presentation}",
      after=".*",
      engine="ICU")
  }

})

