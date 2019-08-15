\name{apply_type_funs}
\alias{apply_type_funs}
\title{apply type funs}
\description{Convert columns of \code{match.mat} using corresponding functions from
\code{type.list}.}
\usage{apply_type_funs(match.mat, type.list)}
\arguments{
  \item{match.mat}{character matrix (matches X groups).}
  \item{type.list}{named list of functions to apply to captured groups.}
}

\value{If \code{type.list} is a list of functions, then return a data.frame
whose columns are defined by calling the functions in \code{type.list} on
the corresponding column of \code{match.mat}. Otherwise just return a
character matrix. If \code{match.mat} does not already have rownames, and
it has a column named "name", then that column will be used for
the rownames, and that column will not be returned.}

\author{Toby Dylan Hocking}





