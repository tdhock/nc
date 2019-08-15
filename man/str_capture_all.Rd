\name{str_capture_all}
\alias{str_capture_all}
\title{All matches from one subject, variable argument syntax}
\description{Extract all matches of a named capture regex pattern from one
subject string.
It is for the common case of extracting
all matches of a regex from a single multi-line text file subject;
for other subjects, str_match_all_named can be used to find all matches.
This function uses
\code{\link{var_args_list}} to analyze the arguments and
str_match_all_named to perform the matching.}
\usage{str_capture_all(subject.vec, ..., nomatch.error = getOption("nc.nomatch.error", 
    TRUE), engine = getOption("nc.engine", "PCRE"))}
\arguments{
  \item{subject.vec}{The subject character vector. We treat elements of subject as
separate lines; i.e. we do the regex matching on the single
subject string formed by pasting together the subject character
vector using newlines as the separator.}
  \item{\dots}{name1=pattern1, fun1, etc, which creates the regex
(?<name1>pattern1) and uses fun1 for conversion. These other
arguments specify the regular expression pattern and must be
character/function/list. All patterns must be character vectors of
length 1. If the pattern is a named argument in R, we will add a
named capture group (?P<name>pattern) in the regex. All patterns
are pasted together to obtain the final pattern used for
matching. Each named pattern may be followed by at most one
function which is used to convert the previous named
pattern. Lists are parsed recursively for convenience.}
  \item{nomatch.error}{if TRUE (default), stop with an error if any subject does not
match; otherwise subjects that do not match are reported as
missing/NA rows of the result.}
  \item{engine}{character string, one of}
}

\value{matrix or data.frame with one row for each match, and one column
for each named group, see str_match_all_named for details.}

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
(match.df <- str_capture_all(
  chr.pos.vec,
  name="chr.*?",
  ":",
  chromStart=int.pattern,
  "-",
  chromEnd=int.pattern))
str(match.df)
match.df["chr1", "chromEnd"]

}
