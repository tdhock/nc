\name{capture_first_df}
\alias{capture_first_df}
\title{Capture first match in columns of a data.frame}
\description{Extract text from several columns of a data.frame, using a
different regular expression for each column. Uses
\code{\link{capture_first_vec}} on each column/pattern indicated in
\code{...} -- argument names are interpreted as column names of subject;
argument values are passed as the pattern to
\code{\link{capture_first_vec}}.}
\usage{capture_first_df(..., 
    nomatch.error = getOption("nc.nomatch.error", 
        TRUE), engine = getOption("nc.engine", 
        "PCRE"))}
\arguments{
  \item{\dots}{subject.df, colName1=list(groupName1=pattern1, fun1, etc),
colName2=list(etc), etc. First (un-named) argument should be a
data.frame with character columns of subjects for matching. The
other arguments need to be named (and the names e.g. colName1 and
colName2 need to be column names of the subject data.frame). The
other argument values specify the regular expression, and must be
character/function/list. All patterns must be character vectors of
length 1. If the pattern is a named argument in R,
it becomes a capture \code{\link{group}} in the regex. All
patterns are pasted together to obtain the final pattern used for
matching. Each named pattern may be followed by at most one
function (e.g. fun1) which is used to convert the previous named
pattern. Lists are parsed recursively for convenience.}
  \item{nomatch.error}{if TRUE (default), stop with an error if any subject does not
match; otherwise subjects that do not match are reported as
missing/NA rows of the result.}
  \item{engine}{character string, one of PCRE, ICU, RE2}
}

\value{data.table with same number of rows as subject, with an additional
column for each named capture \code{\link{group}} specified in \code{...}}

\author{Toby Dylan Hocking}




\examples{

## The JobID column can be match with a complicated regular
## expression, that we will build up from small sub-pattern list
## variables that are easy to understand independently.
(sacct.df <- data.frame(
  JobID = c(
    "13937810_25", "13937810_25.batch",
    "13937810_25.extern", "14022192_[1-3]", "14022204_[4]"),
  Elapsed = c(
    "07:04:42", "07:04:42", "07:04:49",
    "00:00:00", "00:00:00"),
  stringsAsFactors=FALSE))

## Just match the end of the range.
int.pattern <- list("[0-9]+", as.integer)
end.pattern <- list(
  "-",
  task.end=int.pattern)
nc::capture_first_df(sacct.df, JobID=list(
  end.pattern, nomatch.error=FALSE))

## Match the whole range inside square brackets.
range.pattern <- list(
  "[[]",
  task.start=int.pattern,
  end.pattern, "?", #end is optional.
  "[]]")
nc::capture_first_df(sacct.df, JobID=list(
  range.pattern, nomatch.error=FALSE))

## Match either a single task ID or a range, after an underscore.
task.pattern <- list(
  "_",
  list(
    task.id=int.pattern,
    "|",#either one task(above) or range(below)
    range.pattern))
nc::capture_first_df(sacct.df, JobID=task.pattern)

## Match type suffix alone.
type.pattern <- list(
  "[.]",
  type=".*")
nc::capture_first_df(sacct.df, JobID=list(
  type.pattern, nomatch.error=FALSE))

## Match task and optional type suffix.
task.type.pattern <- list(
  task.pattern,
  type.pattern, "?")
nc::capture_first_df(sacct.df, JobID=task.type.pattern)

## Match full JobID and Elapsed columns.
(task.df <- nc::capture_first_df(
  sacct.df,
  JobID=list(
    job=int.pattern,
    task.type.pattern),
  Elapsed=list(
    hours=int.pattern,
    ":",
    minutes=int.pattern,
    ":",
    seconds=int.pattern)))
str(task.df)

}
