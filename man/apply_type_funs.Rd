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

\value{data.table with columns defined by calling the functions in
\code{type.list} on the corresponding column of \code{match.mat}. }

\author{Toby Dylan Hocking}





