\name{capture_all_str}
\alias{capture_all_str}
\title{Capture all matches in a single subject string}
\description{Extract each match of a regex pattern from one subject string. It
is for the common case of extracting all matches of a regex from a
single multi-line text file subject. This function uses
\code{\link{var_args_list}} to analyze the arguments.}
\usage{capture_all_str(subject.vec, ..., engine = getOption("nc.engine", 
    "PCRE"), collapse = "\\n")}
\arguments{
  \item{subject.vec}{The subject character vector. We use paste to \code{collapse} \code{subject.vec}
(by default using newline) and treat it as single character string
to search.}
  \item{\dots}{name1=pattern1, fun1, etc, which creates the regex (pattern1),
uses fun1 for conversion, and creates column name1 in the
output. These arguments specify the regular expression
pattern and must be character/function/list. All patterns must be
character vectors of length 1. If the pattern is a named argument
in R, it becomes a capture group in the
regex. All patterns are pasted together to obtain the final
pattern used for matching. Each named pattern may be followed by
at most one function which is used to convert the previous named
pattern. Lists are parsed recursively for convenience.}
  \item{engine}{character string, one of PCRE, ICU, RE2}
  \item{collapse}{string used with paste to \code{collapse} \code{subject.vec}}
}

\value{data.table with one row for each match, and one column for each
capture group. }

\author{Toby Dylan Hocking}




\examples{

library(nc)

chr.pos.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000-222,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
## By default elements of subject are treated as separate lines (and
## NAs are removed). Named arguments are used to create capture
## groups, and conversion functions such as keep.digits are used to
## convert the previously named group.
int.pattern <- list("[0-9,]+", keep.digits)
(match.dt <- capture_all_str(
  chr.pos.vec,
  chrom="chr.*?",
  ":",
  chromStart=int.pattern,
  "-",
  chromEnd=int.pattern))
str(match.dt)

## use engine="ICU" for unicode character classes
## http://userguide.icu-project.org/strings/regexp e.g. match any
## character with a numeric value of 2 (including japanese etc).
capture_all_str(
  "\\u4e8c \\u4e09 2 3 ",
  two="[\\\\p{numeric_value=2}]",
  engine="ICU")

}
