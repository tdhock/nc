\name{measure_single}
\alias{measure_single}
\title{measure single}
\description{Compute a measure.vars vector (indicating a single output column)
with variable_table attribute to pass to
\code{\link[data.table]{melt.data.table}}.}
\usage{measure_single(subject.names, 
    match.dt, no.match, 
    value.name = NULL)}
\arguments{
  \item{subject.names}{character vector of data frame column names.}
  \item{match.dt}{data table of matches.}
  \item{no.match}{logical vector.}
  \item{value.name}{string.}
}



\author{Toby Dylan Hocking}





