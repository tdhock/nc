str_capture_all <- structure(function # Single string subject, capture all matches
### Extract each match of a regex pattern from one
### subject string.
### It is for the common case of extracting
### all matches of a regex from a single multi-line text file subject.
### This function uses
### var_args_list to analyze the arguments.
(subject.vec,
### The subject character vector. We use paste to collapse subject.vec
### (by default using newline) and treat it as single character string
### to search.
  ...,
### name1=pattern1, fun1, etc, which creates the regex (pattern1),
### uses fun1 for conversion, and creates column name1 in the
### output. These other arguments specify the regular expression
### pattern and must be character/function/list. All patterns must be
### character vectors of length 1. If the pattern is a named argument
### in R, it becomes a capture group in the
### regex. All patterns are pasted together to obtain the final
### pattern used for matching. Each named pattern may be followed by
### at most one function which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
  engine=getOption("nc.engine", "PCRE"),
### character string, one of PCRE, ICU, RE2
  collapse="\n"
### string used with paste to collapse subject.vec
){
  stop_for_subject(subject.vec)
  stop_for_engine(engine)
  L <- var_args_list(...)
  subject <- paste(
    subject.vec[!is.na(subject.vec)],
    collapse=collapse)
  df.args <- lapply(L$fun.list, function(f)f(character()))
  df.args$stringsAsFactors <- FALSE
  no.match.df <- do.call(data.frame, df.args)
  group.mat <- if(engine=="PCRE"){
    try_or_stop_print_pattern({
      vec.with.attrs <- gregexpr(L$pattern, subject, perl=TRUE)[[1]]
    }, L$pattern, engine)
    if(vec.with.attrs[1] == -1)return(no.match.df)
    first <- attr(vec.with.attrs, "capture.start")
    last <- attr(vec.with.attrs, "capture.length")-1+first
    subs <- substring(subject, first, last)
    matrix(subs, nrow=nrow(first))
  }else{
    match.fun <- if(engine=="ICU"){
      stringi::stri_match_all_regex
    }else{
      re2r::re2_match_all
    }
    match.mat <- try_or_stop_print_pattern({
      match.fun(subject, L$pattern)[[1]]
    }, L$pattern, engine)
    never.error <- function(...)NULL
    only_captures(match.mat, never.error)
  }
  apply_type_funs(group.mat, L$fun.list)
### data.frame with one row for each match, and one column for each
### capture group. Row names are taken from the name group.
}, ex=function(){

  chr.pos.vec <- c(
    "chr10:213,054,000-213,055,000",
    "chrM:111,000-222,000",
    "this will not match",
    NA, # neither will this.
    "chr1:110-111 chr2:220-222") # two possible matches.
  keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
  ## str_match_all_variable treats elements of subject as separate
  ## lines (and ignores NA elements). Named arguments are used to
  ## create named capture groups, and conversion functions such as
  ## keep.digits are used to convert the previously named group.
  int.pattern <- list("[0-9,]+", keep.digits)
  (match.df <- nc::str_capture_all(
    chr.pos.vec,
    name="chr.*?",
    ":",
    chromStart=int.pattern,
    "-",
    chromEnd=int.pattern))
  str(match.df)
  match.df["chr1", "chromEnd"]

  ## use engine="ICU" for unicode character classes
  ## http://userguide.icu-project.org/strings/regexp e.g. match any
  ## character with a numeric value of 2 (including japanese etc).
  nc::str_capture_all(
    "\u4e8c \u4e09 2 3 ",
    two="[\\p{numeric_value=2}]",
    engine="ICU")

})

