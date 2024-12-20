capture_first_vec <- structure(function # Capture first match in each character vector element
### Use a regular expression (regex) with capture groups to extract
### the first matching text from each of several subject strings. For
### all matches in one multi-line text file or string use
### capture_all_str. For the first match in every row of a data.frame,
### using a different regex for each column, use capture_first_df. For
### reading regularly named files, use capture_first_glob. For
### matching column names in a wide data frame and then
### melting/reshaping those columns to a taller/longer data frame, see
### capture_melt_single and capture_melt_multiple. To simplify the
### definition of the regex you can use field, quantifier, and
### alternatives.
(...,
### subject, name1=pattern1, fun1, etc. The first argument must be a
### character vector of length>0 (subject strings to parse with a
### regex). Arguments after the first specify the regex/conversion and
### must be string/list/function. All character strings are pasted
### together to obtain the final regex used for matching. Each
### string/list with a named argument in R becomes a capture group in
### the regex, and the name is used for the corresponding column of
### the output data table. Each function must be un-named, and is used
### to convert the previous capture group. Each un-named list becomes
### a non-capturing group. Elements in each list are parsed
### recursively using these rules.
  nomatch.error=getOption("nc.nomatch.error", TRUE),
### if TRUE (default), stop with an error if any subject does not
### match; otherwise subjects that do not match are reported as
### missing/NA rows of the result.
  engine=getOption("nc.engine", "PCRE"),
### character string, one of PCRE, ICU, RE2
  type.convert=getOption("nc.type.convert", FALSE)
### Default conversion function, which will be used on each capture
### group, unless a specific conversion is specified for that
### group. If TRUE, use utils::type.convert; if FALSE, use
### base::identity; otherwise must be a function of at least one
### argument (character), returning an atomic vector of the same
### length.
){
  L <- subject_var_args(..., type.convert=type.convert)
  subject.vec <- L[["subject"]]
  stop_for_engine(engine)
  ##alias<< nc
  stop_for_na <- function(no.match){
    if(isTRUE(nomatch.error) && any(no.match)){
      no.match.i <- which(no.match)
      stop(domain=NA, gettextf("subject(s) %s (%d total) did not match regex below; to output missing rows use nomatch.error=FALSE
%s", collapse_some(no.match.i), length(no.match.i), L[["pattern"]]))
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
      re2::re2_match
    }
    match.mat <- try_or_stop_print_pattern({
      match.fun(subject.vec, L[["pattern"]])
    }, L[["pattern"]], engine)
    only_captures(match.mat, stop_for_na)
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

  ## another subject from https://adventofcode.com/2024/day/14
  ## type.convert=TRUE means to use utils::type.convert as default
  ## conversion function
  pvxy.subject <- c("p=0,4 v=3,-3","p=6,3 v=-1,-3")
  nc::capture_first_vec(
    pvxy.subject,
    "p=",
    px="[0-9]",
    ",",
    py="[0-9]",
    " v=",
    vx="[-0-9]+",
    ",",
    vy="[-0-9]+",
    type.convert=TRUE)

  ## to do the same as above but with less repetition:
  g <- function(prefix,suffix)nc::group(
    name=paste0(prefix,suffix),
    "[-0-9]+")
  xy <- function(prefix)list(
    prefix,
    "=",
    g(prefix,"x"),
    ",",
    g(prefix,"y"))
  nc::capture_first_vec(
    pvxy.subject,
    xy("p"),
    " ",
    xy("v"),
    type.convert=TRUE)

  ## or use a sub-pattern list without type.convert arg:
  ipat <- list("[-0-9]+", as.integer)
  nc::capture_first_vec(
    pvxy.subject,
    "p=",
    px=ipat,
    ",",
    py=ipat,
    " v=",
    vx=ipat,
    ",",
    vy=ipat)

})

