\name{capture_first_df}
\alias{capture_first_df}
\title{Capture first match in columns of a data frame}
\description{Capture first matching text from one or more character columns of
a data frame, using a different regular expression for each
column.}
\usage{capture_first_df(..., 
    nomatch.error = getOption("nc.nomatch.error", 
        TRUE), existing.error = getOption("nc.existing.error", 
        TRUE), engine = getOption("nc.engine", 
        "PCRE"), type.convert = getOption("nc.type.convert", 
        FALSE))}
\arguments{
  \item{\dots}{subject data frame, colName1=list(groupName1=pattern1, fun1, etc),
colName2=list(etc), etc. First argument must be a data frame with
one or more character columns of subjects for matching. If the
first argument is a data table then it will be modified using
\code{\link[data.table]{set}} (for memory efficiency, to avoid copying the whole
data table); otherwise the input data frame will be copied to a
new data table. Each other argument must be named using a column
name of the subject data frame, e.g. colName1, colName2. Each
other argument value must be a list that specifies the
regex/conversion to use (in string/function/list format as
documented in \code{\link{capture_first_vec}}, which is used on each named
column), and possibly a column-specific \code{engine} to use.}
  \item{nomatch.error}{if TRUE (default), stop with an error if any subject does not
match; otherwise subjects that do not match are reported as
missing/NA rows of the result.}
  \item{existing.error}{if TRUE (default to avoid data loss), stop with an error if any
capture groups have the same name as an existing column of
subject.}
  \item{engine}{character string, one of PCRE, ICU, RE2. This \code{engine} will be used
for each column, unless another \code{engine} is specified for that
column in \code{...}}
  \item{type.convert}{Default conversion function, which will be used on each capture
\code{\link{group}}, unless a specific conversion is specified for that
\code{\link{group}}. If TRUE, use \code{\link[utils]{type.convert}}; if FALSE, use
\code{\link[base]{identity}}; otherwise must be a function of at least one
argument (character), returning an atomic vector of the same
length.}
}

\value{data.table with same number of rows as subject, with an additional
column for each named capture \code{\link{group}} specified in \code{...}}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




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
nc::capture_first_df(
  sacct.df,
  JobID=list(
    job=int.pattern,
    task.type.pattern),
  Elapsed=list(
    hours=int.pattern,
    ":",
    minutes=int.pattern,
    ":",
    seconds=int.pattern))

## If input is data table then it is modified for memory efficiency,
## to avoid copying entire table.
sacct.DT <- data.table::as.data.table(sacct.df)
nc::capture_first_df(sacct.df, JobID=task.pattern)
sacct.df #not modified.
nc::capture_first_df(sacct.DT, JobID=task.pattern)
sacct.DT #modified!

}
