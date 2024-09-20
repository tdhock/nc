\name{altlist}
\alias{altlist}
\title{altlist}
\description{Create a named list containing named patterns, useful for creating
a named list of named sub-pattern groups to be used with
\code{\link{alternatives}}. This function is used to implement the more
user-friendly \code{\link{alternatives_with_shared_groups}}, which should be
preferred.}
\usage{altlist(...)}
\arguments{
  \item{\dots}{Named patterns which will be used as sub-pattern groups in \code{\link{alternatives}}.}
}

\value{Named list of patterns to be used for constructing \code{\link{alternatives}}
using \code{\link[base]{with}}, see examples.}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

## Example 1: matching dates in different formats, but always same
## type in each alternative.
subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
pat.list <- nc::altlist(month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}")
pattern <- with(pat.list, nc::alternatives(
  american=list(month, " ", day, ", ", year),
  european=list(day, " ", month, " ", year)))
match.dt <- nc::capture_first_vec(subject.vec, pattern)
print(match.dt, class=TRUE)
match.dt[, data.table::as.IDate(paste0(month, day, year), format="\%b\%d\%Y")]

## Example 2: matching dates in different formats, but with
## different types in different alternatives.
subject.vec <- c("3/17/1983", "26 sep 2017")
month2int <- c(#this approach is locale-indepdendent.
  jan=1L, feb=2L, mar=3L, apr=4L,  may=5L,  jun=6L,
  jul=7L, aug=8L, sep=9L, oct=10L, nov=11L, dec=12L)
pat.list <- nc::altlist(
  day=list("[0-9]{2}", as.integer),
  year=list("[0-9]{4}", as.integer))
pattern <- with(pat.list, nc::alternatives(
  american=list(month="[0-9]", as.integer, "/", day, "/", year),
  european=list(
    day, " ", month="[a-z]{3}", function(m)month2int[m], " ", year)))
match.dt <- nc::capture_first_vec(subject.vec, pattern)
print(match.dt, class=TRUE)

}
