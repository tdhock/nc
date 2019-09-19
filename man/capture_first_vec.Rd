\name{capture_first_vec}
\alias{capture_first_vec}
\alias{nc}
\title{Capture first match in each character vector element}
\description{Extract the first match of a regex pattern from each
of several subject strings. This function uses \code{\link{var_args_list}}
to analyze the arguments.
For the first match in every row of a data.frame, using
a different regex for each column, use \code{\link{capture_first_df}}. For all
matches in one multi-line text file use \code{\link{capture_all_str}}.
To avoid repetition when a \code{\link{group}} name is also used in the pattern,
use \code{\link{field}}.}
\usage{capture_first_vec(subject.vec, ..., nomatch.error = getOption("nc.nomatch.error", 
    TRUE), engine = getOption("nc.engine", "PCRE"))}
\arguments{
  \item{subject.vec}{The subject character vector.}
  \item{\dots}{name1=pattern1, fun1, etc, which creates the regex (pattern1),
uses fun1 for conversion, and creates column name1 in the
output. These arguments specify the regular expression
pattern and must be character/function/list. All patterns must be
character vectors of length 1. If the pattern is a named argument
in R, it becomes a capture \code{\link{group}} in the
regex. All patterns are pasted together to obtain the final
pattern used for matching. Each named pattern may be followed by
at most one function which is used to convert the previous named
pattern. Lists are parsed recursively for convenience.}
  \item{nomatch.error}{if TRUE (default), stop with an error if any subject does not
match; otherwise subjects that do not match are reported as
missing/NA rows of the result.}
  \item{engine}{character string, one of PCRE, ICU, RE2}
}

\value{data.table with one row for each subject, and one column for each
capture \code{\link{group}}. }

\author{Toby Dylan Hocking}




\examples{

library(nc)

named.subject.vec <- c(
  ten="chr10:213,054,000-213,055,000",
  M="chrM:111,000",
  one="chr1:110-111 chr2:220-222") # two possible matches.
## Find the first match in each element of the subject character
## vector. Named argument values are used to create capture groups
## in the generated regex, and argument names become column names in
## the result. 
(dt.chr.cols <- capture_first_vec(
  named.subject.vec,
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
(dt.int.cols <- capture_first_vec(
  named.subject.vec, range.pattern))

## Conversion functions used to create non-char columns.
str(dt.int.cols)

## NA used to indicate no match or missing subject.
na.vec <- c(
  nomatch="this will not match",
  missing=NA, # neither will this.
  named.subject.vec)
capture_first_vec(na.vec, range.pattern, nomatch.error=FALSE)

## alternate regex engine, but this example with emoji only works
## with recent versions of ICU.
if(requireNamespace("stringi") && stringi::stri_info()$ICU.version >= 59){
  capture_first_vec(
    "foo a\\U0001F60E# bar",
    before=".*?",
    emoji="\\\\p{EMOJI_Presentation}",
    after=".*",
    engine="ICU")
}

}
