\name{capture_melt_single}
\alias{capture_melt_single}
\title{Capture and melt into a single column}
\description{Match a regex to column names of a wide data frame (many
columns/few rows), then melt/reshape the matching columns into a
single result column in a taller/longer data table (fewer columns/more
rows). It is for the common case of melting several columns of
the same type in a "wide" input data table which has several
distinct pieces of information encoded in each column name. For
melting into several result columns of possibly different types,
see \code{\link{capture_melt_multiple}}.}
\usage{capture_melt_single(..., 
    value.name = "value", 
    na.rm = TRUE, verbose = getOption("datatable.verbose"))}
\arguments{
  \item{\dots}{First argument must be a data frame to melt/reshape; column names
of this data frame will be used as the subjects for regex
matching. Other arguments (regex/conversion/engine) are passed to
\code{\link{capture_first_vec}} along with nomatch.error=FALSE.}
  \item{value.name}{Name of the column in output which has values taken from
melted/reshaped column values of input (passed to
\code{\link[data.table]{melt.data.table}}).}
  \item{na.rm}{remove missing values from melted data? (passed to
\code{\link[data.table]{melt.data.table}})}
  \item{verbose}{Print \code{verbose} output messages? (passed to
\code{\link[data.table]{melt.data.table}})}
}
\details{As in \code{\link[data.table]{melt.data.table}}, the order of the
output columns is: first the columns copied from input (which did
not match the specified pattern), then columns captured from
variable names, and finally the value column.

\code{\link[data.table]{melt.data.table}} is called to perform the
melt operation, with measure.vars = the column names
that matched the specified regex, and id.vars = the other column
names (which did not match).}
\value{Data table of reshaped/melted/tall/long data, with a new column for each named
argument in the pattern, and additionally variable/value columns.}

\author{Toby Dylan Hocking}



\seealso{This function is inspired by tidyr::pivot_longer which
requires some repetition, i.e. the columns to melt and pattern to
match the melted column names must be specified in separate
arguments. In contrast capture_melt_single uses the specified
pattern for both purposes, which avoids some repetition in user
code.}
\examples{

## Example 1: melt iris data and barplot for each numeric variable.
(iris.tall <- nc::capture_melt_single(
  iris,
  part=".*",
  "[.]",
  dim=".*",
  value.name="cm"))
## Histogram of cm for each variable.
if(require("ggplot2")){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(part ~ dim)+
    geom_bar(aes(cm), data=iris.tall)
}

## Example 2: melt who data and use type conversion functions for
## year limits (e.g. for censored regression).
if(requireNamespace("tidyr")){
  data(who, package="tidyr", envir=environment())
  ##2.1 just extract diagnosis and gender to chr columns.
  new.diag.gender <- list(#save pattern as list for re-use later.
    "new_?",
    diagnosis=".*",
    "_",
    gender=".")
  who.tall.chr <- nc::capture_melt_single(who, new.diag.gender, na.rm=TRUE)
  print(head(who.tall.chr))
  str(who.tall.chr)
  ##2.2 also extract ages and convert to numeric output columns.
  who.tall.num <- nc::capture_melt_single(
    who,
    new.diag.gender,#previous pattern for matching diagnosis and gender.
    ages=list(#new pattern for matching age range.
      min.years="0|[0-9]{2}", as.numeric,#in-line type conversion functions.
      max.years="[0-9]{0,2}", function(x)ifelse(x=="", Inf, as.numeric(x))),
    value.name="count",
    na.rm=TRUE)
  print(head(who.tall.num))
  str(who.tall.num)
  ##2.3 compute total count for each age range then display the
  ##subset with max.years lower than a threshold.
  who.age.counts <- who.tall.num[, .(
    total=sum(count)
  ), by=.(min.years, max.years)]
  print(who.age.counts[max.years < 50])
}

## Example 3: pepseq data.
if(requireNamespace("R.utils")){#for reading gz files with data.table
  pepseq.dt <- data.table::fread(
    system.file("extdata", "pepseq.txt.gz", package="nc", mustWork=TRUE))
  u.pepseq <- pepseq.dt[, unique(names(pepseq.dt)), with=FALSE]
  nc::capture_melt_single(
    u.pepseq,
    "^",
    prefix=".*?",
    nc::field("D", "", ".*?"),
    "[.]",
    middle=".*?",
    "[.]",
    "[0-9]+",
    suffix=".*",
    "$")
}

}
