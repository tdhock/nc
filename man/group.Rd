\name{group}
\alias{group}
\title{Capture group}
\description{Create a capture group (named column in output). In the vast
majority of patterns R arguments can/should be used to specify
names, e.g. list(name=pattern). This is a helper function which is
useful for programmatically creating group names (see example for
a typical use case).}
\usage{group(name, ...)}
\arguments{
  \item{name}{Column \code{name} in output.}
  \item{\dots}{Regex pattern(s).}
}

\value{Named list.}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

## Three ways to create a group named data which matches zero or
## more non-newline characters.
str(list(data=".*"))
str(nc::group("data", ".*"))
g.name <- "data"
str(nc::group(g.name, ".*"))

## Data downloaded from
## https://en.wikipedia.org/wiki/Hindu\%E2\%80\%93Arabic_numeral_system
numerals <- system.file(
  "extdata", "Hindu-Arabic-numerals.txt.gz", package="nc")

## Use engine="ICU" for unicode character classes
## http://userguide.icu-project.org/strings/regexp e.g. match any
## character with a numeric value of 2 (including japanese etc).
if(requireNamespace("stringi"))
  nc::capture_all_str(
  numerals,
  " ",
  two="[\\\\p{numeric_value=2}]",
  " ",
  engine="ICU")

## Create a table of numerals with script names.
digits.pattern <- list()
for(digit in 0:9){
  digits.pattern[[length(digits.pattern)+1]] <- list(
    "[|]",
    nc::group(paste(digit), "[^{|]+"),
    "[|]")
}
nc::capture_all_str(
  numerals,
  "\\n",
  digits.pattern,
  "[|]",
  " *",
  "\\\\[\\\\[",
  name="[^\\\\]|]+")

}
