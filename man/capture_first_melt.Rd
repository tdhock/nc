\name{capture_first_melt}
\alias{capture_first_melt}
\title{Capture column names and melt}
\description{Attempt to match a regex to \code{subject.df} column names,
then melt the matching columns to a single
result column in a tall data table,
and add output columns for each \code{\link{group}} defined in the regex.
It is for the common case of melting
several columns of the same type in a "wide" input data table which has
several distinct pieces of information encoded in each column
name. For melting into several result columns of different types, see
\code{\link{capture_first_melt_multiple}}.}
\usage{capture_first_melt(subject.df, ..., id.vars = NULL, 
    variable.name = "variable", value.name = "value", 
    na.rm = FALSE, verbose = getOption("datatable.verbose"))}
\arguments{
  \item{subject.df}{The data.frame with column name subjects.}
  \item{\dots}{Pattern passed to \code{\link{capture_first_vec}}.}
  \item{id.vars}{Columns to copy to the output data table (passed to
\code{\link[data.table]{melt.data.table}}). Default NULL means to use all
columns not matched by the pattern.}
  \item{variable.name}{Name of the column in output which has values taken from melted
column names of input (passed to \code{\link[data.table]{melt.data.table}}).}
  \item{value.name}{Name of the column in output which has values taken from melted
column values of input (passed to \code{\link[data.table]{melt.data.table}}).}
  \item{na.rm}{remove missing values from melted data? (passed to
\code{\link[data.table]{melt.data.table}})}
  \item{verbose}{Print \code{verbose} output messages? (passed to
\code{\link[data.table]{melt.data.table}})}
}
\details{\code{\link{capture_first_vec}} is called to perform regex matching on
the input column names.

\code{\link[data.table]{melt.data.table}} is called to perform the
melt operation.}
\value{Data table of melted/tall data, with a new column for each named
argument in the pattern, and additionally variable/value columns.}

\author{Toby Dylan Hocking}



\seealso{This function is inspired by tidyr::pivot_longer which
requires some repetition, i.e. the columns to melt and pattern to
match the melted column names must be specified in separate
arguments. In contrast capture_first_melt uses the specified
pattern for both purposes, which avoids some repetition in user
code.}
\examples{

## Example 1: melt and then dcast iris data, as in cdata package,
## https://winvector.github.io/cdata/
library(data.table)
iris.dt <- data.table(observation=1:nrow(iris), iris)
(iris.tall <- nc::capture_first_melt(
  iris.dt,
  part=".*",
  "[.]",
  dim=".*"))

## Are sepals bigger than petals? YES.
(iris.part.cols <- dcast(
  iris.tall,
  observation + Species + dim ~ part))
iris.part.cols[Sepal<Petal]

## Are the flower longer or wider? LONGER (by definition...)
(iris.dim.cols <- dcast(
  iris.tall,
  observation + Species + part ~ dim))
iris.dim.cols[Length < Width]

}
