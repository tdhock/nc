\name{field}
\alias{field}
\title{Capture a field}
\description{Capture a field with a pattern of the form
list("field.name", \code{between.pattern},
field.name=list(\code{...})) -- see examples.}
\usage{field(field.name, between.pattern, ...)}
\arguments{
  \item{field.name}{Field name, used as a pattern and as a capture \code{\link{group}} (output
column) name.}
  \item{between.pattern}{Pattern to match after \code{field.name} but before the field value.}
  \item{\dots}{Pattern(s) for matching field value.}
}

\value{Pattern list which can be used in \code{\link{capture_first_vec}},
\code{\link{capture_first_df}}, or \code{\link{capture_all_str}}.}

\author{Toby Dylan Hocking}




\examples{

info.txt.gz <- system.file(
  "extdata", "SweeD_Info.txt.gz", package="nc")
info.vec <- readLines(info.txt.gz)
info.vec[24:40]
## For each Alignment there are many fields which have a similar
## pattern, and occur in the same order. One way to capture these
## fields is by coding a pattern that says to look for all of those
## fields in that order. Each field is coded using this helper
## function.
g <- function(name, fun=identity, suffix=list()){
  list(
    "\\t+",
    nc::field(name, ":\\t+", ".*"),
    fun,
    suffix,
    "\\n+")
}
nc::capture_all_str(
  info.vec,
  nc::field("Alignment", " ", "[0-9]+"),
  "\\n+",
  g("Chromosome"),
  g("Sequences", as.integer),
  g("Sites", as.integer),
  g("Discarded sites", as.integer),
  g("Processing", as.integer, " seconds"),
  g("Position", as.integer),
  g("Likelihood", as.numeric),
  g("Alpha", as.numeric))

## Another example where field is useful. 
trackDb.txt.gz <- system.file(
  "extdata", "trackDb.txt.gz", package="nc")
trackDb.vec <- readLines(trackDb.txt.gz)
cat(trackDb.vec[101:115], sep="\\n")
int.pattern <- list("[0-9]+", as.integer)
 cell.sample.type <- list(
  cellType="[^ ]*?",
  "_",
  sampleName=list(
    "McGill",
    sampleID=int.pattern),
  dataType="Coverage|Peaks")
## Each block in the trackDb file begins with track, followed by a
## space, followed by the track name. That pattern is coded below,
## using field:
track.pattern <- nc::field(
  "track",
  " ",
  cell.sample.type,
  "|",
  "[^\\n]+")
nc::capture_all_str(trackDb.vec, track.pattern)

## Each line in a block has the same structure (field name, space,
## field value). Below we use the field function to extract the
## color line, along with columns for each of the three channels
## (red, green, blue).
any.lines.pattern <- "(?:\\n[^\\n]+)*"
nc::capture_all_str(
  trackDb.vec,
  track.pattern,
  any.lines.pattern,
  "\\\\s+",
  nc::field(
    "color", " ",
    red=int.pattern, ",",
    green=int.pattern, ",",
    blue=int.pattern))

}
