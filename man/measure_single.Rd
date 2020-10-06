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
  \item{subject.names}{
}
  \item{match.dt}{
}
  \item{no.match}{
}
  \item{value.name}{
}
}
\details{\code{\link[data.table]{melt.data.table}} is called to perform the
melt operation, with measure.vars = the column names
that matched the specified regex, and id.vars = the other column
names (which did not match).}


\author{Toby Dylan Hocking}





