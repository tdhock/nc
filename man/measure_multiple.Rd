\name{measure_multiple}
\alias{measure_multiple}
\title{measure multiple}
\description{Compute a measure.vars list (indicating multiple output columns)
with variable_table attribute to pass to
\code{\link[data.table]{melt.data.table}}.}
\usage{measure_multiple(subject.names, 
    match.dt, no.match, 
    fill = TRUE)}
\arguments{
  \item{subject.names}{character vector of data frame column names.}
  \item{match.dt}{data table of matches.}
  \item{no.match}{logical vector.}
  \item{fill}{logical.}
}



\author{Toby Dylan Hocking}





