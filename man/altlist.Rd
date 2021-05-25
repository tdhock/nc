\name{altlist}
\alias{altlist}
\title{altlist}
\description{Create a named list containing named patterns, useful when
\code{\link{alternatives}} have common sub-patterns.}
\usage{altlist(...)}
\arguments{
  \item{\dots}{Named patterns.}
}

\value{Named list of patterns to be used for constructing \code{\link{alternatives}}
using \code{\link[base]{with}}, see examples.}

\author{Toby Dylan Hocking}




\examples{

## Example 1: matching dates in different formats, but always same
## type in each alternative.
subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
## One way to do it would be with alternative format strings.
Sys.setlocale(locale="C")
idate.mat <- sapply(
  c("\%b \%d, \%Y", "\%d \%b \%Y"),
  function(f)data.table::as.IDate(subject.vec, format=f))
col.vec <- apply(!is.na(idate.mat), 1, which)
i.mat <- cbind(1:nrow(idate.mat), col.vec)
data.table::as.IDate(idate.mat[i.mat])
## Another way is with alternative regular expressions.
pat.list <- nc::altlist(month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}")
pattern <- with(pat.list, nc::alternatives(
  list(month, " ", day, ", ", year),
  list(day, " ", month, " ", year)))
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
  list(month="[0-9]", as.integer, "/", day, "/", year),
  list(day, " ", month="[a-z]{3}", function(m)month2int[m], " ", year)))
match.dt <- nc::capture_first_vec(subject.vec, pattern)
print(match.dt, class=TRUE)

## Example 3: three alternatives with four groups each.
subject.vec <- c(
  "EUR 5.00 Theft in delivery inserted in wire transfer 11/02/2021",
  "EUR 50.00 - Refund for theft in delivery - 30/07/2020",
  "EUR68.50 - Refund for theft in delivery 02/07/2020",
  "45.00 EUR 29/10/2020 Refund for theft in delivery",
  "53.00\\u20ac Refund for theft in delivery 24/09/2020")
pat.list <- nc::altlist(
  currency="EUR|\\u20ac",
  amount=list("[0-9.]+", as.numeric),
  reason="[A-Za-z ]+?",
  date=list(
    "[0-9]{2}/[0-9]{2}/[0-9]{4}",
    function(d)data.table::as.IDate(d, format="\%d/\%m/\%Y")))
sep <- function(x, y, ...){
  if(missing(y)){
    list("^", x, "$")
  }else{
    sep(list(x, list(" - | |"), y), ...)
  }
}
pattern <- with(pat.list, nc::alternatives(
  sep(currency, amount, reason, date),
  sep(amount, currency, date, reason),
  sep(amount, currency, reason, date)))
match.dt <- nc::capture_first_vec(subject.vec, pattern)
print(match.dt, class=TRUE)

}
