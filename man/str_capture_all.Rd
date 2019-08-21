\name{str_capture_all}
\alias{str_capture_all}
\title{Single string subject, capture all matches}
\description{Extract each match of a regex pattern from one
subject string.
It is for the common case of extracting
all matches of a regex from a single multi-line text file subject.
This function uses
\code{\link{var_args_list}} to analyze the arguments.}
\usage{str_capture_all(subject.vec, ..., engine = getOption("nc.engine", 
    "PCRE"), collapse = "\\n")}
\arguments{
  \item{subject.vec}{The subject character vector. We use paste to \code{collapse} \code{subject.vec}
(by default using newline) and treat it as single character string
to search.}
  \item{\dots}{name1=pattern1, fun1, etc, which creates the regex (pattern1),
uses fun1 for conversion, and creates column name1 in the
output. These other arguments specify the regular expression
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

\value{data.frame with one row for each match, and one column for each
capture group. Row names are taken from the name group.}

\author{Toby Dylan Hocking}




\examples{

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
  "\\u4e8c \\u4e09 2 3 ",
  two="[\\\\p{numeric_value=2}]",
  engine="ICU")

}
