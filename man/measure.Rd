\name{measure}
\alias{measure}
\title{measure}
\description{Computes a value to be used as measure.vars argument to
\code{\link[data.table]{melt.data.table}}. NOTE: only works on newer versions of
data.table that include the measure function.}
\usage{measure(..., cols)}
\arguments{
  \item{\dots}{Regular expression pattern list, passed to \code{\link{capture_first_vec}} with
\code{cols} as subject.}
  \item{cols}{Character vector, column names to match with regex.}
}
\details{\code{\link{measure_multiple}} is called if there is a capture \code{\link{group}}
named "column" and \code{\link{measure_single}} is called otherwise.}
\value{List or vector to use as measure.vars argument to
\code{\link[data.table]{melt.data.table}}.}

\author{Toby Dylan Hocking}




\examples{

library(data.table)
iris.dt <- data.table(datasets::iris[c(1,150),])
melt(iris.dt, measure=nc::measure(part  =".*", "[.]", dim   =".*"))
melt(iris.dt, measure=nc::measure(column=".*", "[.]", dim   =".*"))
melt(iris.dt, measure=nc::measure(part  =".*", "[.]", column=".*"))

}
