str_capture_all <- structure(function # All matches from one subject, variable argument syntax
### Extract all matches of a named capture regex pattern from one
### subject string.
### It is for the common case of extracting
### all matches of a regex from a single multi-line text file subject;
### for other subjects, str_match_all_named can be used to find all matches.
### This function uses
### var_args_list to analyze the arguments and
### str_match_all_named to perform the matching.
(subject.vec,
### The subject character vector. We treat elements of subject as
### separate lines; i.e. we do the regex matching on the single
### subject string formed by pasting together the subject character
### vector using newlines as the separator.
  ...,
### name1=pattern1, fun1, etc, which creates the regex
### (?<name1>pattern1) and uses fun1 for conversion. These other
### arguments specify the regular expression pattern and must be
### character/function/list. All patterns must be character vectors of
### length 1. If the pattern is a named argument in R, we will add a
### named capture group (?P<name>pattern) in the regex. All patterns
### are pasted together to obtain the final pattern used for
### matching. Each named pattern may be followed by at most one
### function which is used to convert the previous named
### pattern. Lists are parsed recursively for convenience.
  nomatch.error=getOption("nc.nomatch.error", TRUE),
### if TRUE (default), stop with an error if any subject does not
### match; otherwise subjects that do not match are reported as
### missing/NA rows of the result.
  engine=getOption("nc.engine", "PCRE")
### character string, one of
){
  stop_for_subject(subject.vec)
  stop_for_engine(engine)
  L <- var_args_list(...)
  subject <- paste(
    subject.vec[!is.na(subject.vec)],
    collapse="\n")
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
### matrix or data.frame with one row for each match, and one column
### for each named group, see str_match_all_named for details.
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
  (match.df <- str_capture_all(
    chr.pos.vec,
    name="chr.*?",
    ":",
    chromStart=int.pattern,
    "-",
    chromEnd=int.pattern))
  str(match.df)
  match.df["chr1", "chromEnd"]

})

