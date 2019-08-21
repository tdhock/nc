\name{vec_capture_first}
\alias{vec_capture_first}
\alias{nc}
\title{Character vector subject, capture first match}
\description{Extract the first match of a regex pattern from each
of several subject strings. This function uses \code{\link{var_args_list}}
to analyze the arguments.
For the first match in every row of a data.frame, using
a different regex for each column, use \code{\link{df_capture_first}}. For all
matches in one multi-line text file use \code{\link{str_capture_all}}.}
\usage{vec_capture_first(subject.vec, ..., nomatch.error = getOption("nc.nomatch.error", 
    TRUE), engine = getOption("nc.engine", "PCRE"))}
\arguments{
  \item{subject.vec}{The subject character vector.}
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
  \item{nomatch.error}{if TRUE (default), stop with an error if any subject does not
match; otherwise subjects that do not match are reported as
missing/NA rows of the result.}
  \item{engine}{character string, one of PCRE, ICU, RE2}
}

\value{data.frame with one row for each subject, and one column for each
capture group. Row names are taken from names of \code{subject.vec} or
the name group.}

\author{Toby Dylan Hocking}




\examples{

named.subject.vec <- c(
  ten="chr10:213,054,000-213,055,000",
  M="chrM:111,000",
  one="chr1:110-111 chr2:220-222") # two possible matches.
## vec_capture_first finds the first match in each element of the
## subject character vector. Named arguments are used to create
## named capture groups, which become column names in the
## result. Since the subject is named, those names are used for the
## rownames of the result.
(df.chr.cols <- nc::vec_capture_first(
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

## Rownames taken from subject if it has names.
(df.int.cols <- nc::vec_capture_first(
  named.subject.vec, range.pattern))

## Conversion functions used to create non-char columns.
str(df.int.cols)

## Rownames taken from name group if subject is un-named.
nc::vec_capture_first(
  unname(named.subject.vec), range.pattern)

## NA used to indicate no match or missing subject.
na.vec <- c(
  nomatch="this will not match",
  missing=NA, # neither will this.
  named.subject.vec)
nc::vec_capture_first(
  na.vec, range.pattern, nomatch.error=FALSE)

## alternate regex engine, but this example with emoji only works
## with ICU > 59.
if(interactive()){
  nc::vec_capture_first(
    "foo a\\U0001F60E# bar",
    before=".*?",
    emoji="\\\\p{EMOJI_Presentation}",
    after=".*",
    engine="ICU")
}

}
