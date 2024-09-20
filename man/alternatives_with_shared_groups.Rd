\name{alternatives_with_shared_groups}
\alias{alternatives_with_shared_groups}
\title{alternatives with shared groups}
\description{Create a pattern which matches \code{\link{alternatives}} with common
sub-pattern groups.}
\usage{alternatives_with_shared_groups(...)}
\arguments{
  \item{\dots}{named arguments are sub-pattern groups, un-named arguments are
alternative patterns which can refer to argument names.}
}

\value{Pattern created by first using \code{\link{altlist}} on named arguments to
create a list of sub-patterns, then using \code{\link{alternatives}} on un-named
arguments (evaluated using the names defined in the sub-pattern
list).}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

## Example 0: matching family and given names.
nc::capture_first_vec(
  c("Toby Dylan Hocking","Hocking, Toby Dylan"),
  nc::alternatives_with_shared_groups(
    family="[A-Z][a-z]+",
    given="[^,]+",
    list(given, " ", family),
    list(family, ", ", given)
  )
)

## Example 1: matching dates in different formats, but always same
## type in each alternative.
subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
## Another way is with alternative regular expressions.
pattern <- nc::alternatives_with_shared_groups(
  month="[a-z]{3}",
  day="[0-9]{2}",
  year="[0-9]{4}",
  list(american=list(month, " ", day, ", ", year)),
  list(european=list(day, " ", month, " ", year)))
(match.dt <- nc::capture_first_vec(subject.vec, pattern))
match.dt[, data.table::as.IDate(paste0(month, day, year), format="\%b\%d\%Y")]
## american and european columns can be searched to see which
## alternative matched.
match.dt[european!=""]

## Example 2: matching dates in different formats, but with
## different types in different alternatives.
subject.vec <- c("3/17/1983", "26 sep 2017")
month2int <- c(#this approach is locale-indepdendent.
  jan=1L, feb=2L, mar=3L, apr=4L,  may=5L,  jun=6L,
  jul=7L, aug=8L, sep=9L, oct=10L, nov=11L, dec=12L)
pattern <- nc::alternatives_with_shared_groups(
  day=list("[0-9]{2}", as.integer),
  year=list("[0-9]{4}", as.integer),
  list(month="[0-9]", as.integer, "/", day, "/", year),
  list(day, " ", month="[a-z]{3}", function(m)month2int[m], " ", year))
match.dt <- nc::capture_first_vec(subject.vec, pattern)
print(match.dt, class=TRUE)

## Example 3: three alternatives with four groups each.
subject.vec <- c(
  "EUR 5.00 Theft in delivery inserted in wire transfer 11/02/2021",
  "EUR 50.00 - Refund for theft in delivery - 30/07/2020",
  "EUR68.50 - Refund for theft in delivery 02/07/2020",
  "45.00 EUR 29/10/2020 Refund for theft in delivery",
  "53.00\\u20ac Refund for theft in delivery 24/09/2020")
sep <- function(x, y, ...){
  if(missing(y)){
    list("^", x, "$")
  }else{
    sep(list(x, list(" - | |"), y), ...)
  }
}
pattern <- nc::alternatives_with_shared_groups(
  currency="EUR|\\u20ac",
  amount=list("[0-9.]+", as.numeric),
  reason="[A-Za-z ]+?",
  date=list(
    "[0-9]{2}/[0-9]{2}/[0-9]{4}",
    function(d)data.table::as.IDate(d, format="\%d/\%m/\%Y")),
  sep(currency, amount, reason, date),
  sep(amount, currency, date, reason),
  sep(amount, currency, reason, date))
match.dt <- nc::capture_first_vec(subject.vec, pattern)
print(match.dt, class=TRUE)

}
