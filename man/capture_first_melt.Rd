\name{capture_first_melt}
\alias{capture_first_melt}
\title{Capture column names and melt}
\description{Extract first match of a regex from data.frame column names, by
calling \code{\link{capture_first_vec}} on column names, then using the column
names that matched as measure.vars in \code{\link[data.table]{melt.data.table}},
then joining the two results. It is for the common case of melting
several columns of the same type in a "wide" data table which has
several distinct pieces of information encoded in each column
name. For melting columns of different types, see
\code{\link{capture_first_melt_multiple}}.}
\usage{capture_first_melt(subject.df, ..., id.vars = NULL, 
    variable.name = "variable", value.name = "value", 
    na.rm = FALSE, verbose = getOption("datatable.verbose"))}
\arguments{
  \item{subject.df}{The data.frame with column name subjects.}
  \item{\dots}{Pattern passed to \code{\link{capture_first_vec}}.}
  \item{id.vars}{Columns which should not be melted (passed to
\code{\link[data.table]{melt.data.table}}). Default NULL means to use all
columns which do not match the specified pattern.}
  \item{variable.name}{Name of the column in output which has values taken from melted
column names of input (passed to \code{\link[data.table]{melt.data.table}}).}
  \item{value.name}{Name of the column in output which has values taken from melted
column values of input (passed to \code{\link[data.table]{melt.data.table}}).}
  \item{na.rm}{remove missing values from melted data? (passed to
\code{\link[data.table]{melt.data.table}})}
  \item{verbose}{Print \code{verbose} output messages? (passed to
\code{\link[data.table]{melt.data.table}})}
}

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
if(require("ggplot2")){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(dim ~ Species)+
    coord_equal()+
    geom_abline(slope=1, intercept=0, color="grey")+
    geom_point(aes(
      Petal, Sepal),
      data=iris.part.cols)
}

## Are the flower longer or wider? LONGER (by definition...)
(iris.dim.cols <- dcast(
  iris.tall,
  observation + Species + part ~ dim))
iris.dim.cols[Length < Width]
if(require("ggplot2")){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(part ~ Species)+
    coord_equal()+
    geom_abline(slope=1, intercept=0, color="grey")+
    geom_point(aes(
      Width, Length),
      data=iris.dim.cols)
}

## Example 2: WHO data inspired from Hadley's talk
## https://www.youtube.com/watch?v=qFRYnKdLz5U
if(requireNamespace("tidyr")){
  data(who, package="tidyr", envir=environment())
  who.tall <- nc::capture_first_melt(
    who,
    "new_?",
    diagnosis=".*",
    "_",
    gender=".",
    ages=list(
      min.years="0|[0-9]{2}", as.numeric,
      max.years=list("[0-9]{2}"), "?",
      function(x)ifelse(x=="", Inf, as.numeric(x))),
    value.name="count",
    variable.name="column",
    na.rm=TRUE)
  print(who.tall)
  print(who.tall[, table(diagnosis, gender)])
  print(who.tall[, .(count=.N), by=.(ages, min.years, max.years)])
}

}
