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

\author{Toby Dylan Hocking}




\examples{

info.txt.gz <- system.file(
"extdata", "SweeD_Info.txt.gz", package="nc")
info.vec <- readLines(info.txt.gz)
info.vec[24:40]
## For each Alignment there are many fields which have a similar
## pattern, and occur in the same order. One way to capture these
## fields is by coding a pattern that says to look for all of those
## fields in that order. Each field is defined using this helper
## function.
g <- function(name, fun=identity, suffix=list()){
  list(
    "\\t+",
    name,
    ":\\t+",
    nc::group(name, ".*"),
    fun,
    suffix,
    "\\n+")
}
nc::capture_all_str(
  info.vec,
  "Alignment ",
  alignment="[0-9]+",
  "\\n+",
  g("Chromosome"),
  g("Sequences", as.integer),
  g("Sites", as.integer),
  g("Discarded sites", as.integer),
  g("Processing", as.integer, " seconds"),
  g("Position", as.integer),
  g("Likelihood", as.numeric),
  g("Alpha", as.numeric))

}
