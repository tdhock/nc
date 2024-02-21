\name{apply_type_funs}
\alias{apply_type_funs}
\title{apply type funs}
\description{Convert columns of \code{match.mat} using corresponding functions from
\code{fun.list}, then handle any duplicate capture \code{\link{group}} names.}
\usage{apply_type_funs(match.mat, 
    fun.list)}
\arguments{
  \item{match.mat}{Character matrix (matches X groups).}
  \item{fun.list}{Named list of functions to apply to captured groups. If there are
any duplicate names, they must be in \code{\link{alternatives}} (only one match
per unique \code{\link{group}} name, otherwise error).}
}

\value{data.table with columns defined by calling the functions in
\code{fun.list} on the corresponding column of \code{match.mat}. Even if
\code{fun.list} has duplicated names, the output data.table will have
unique column names (identically named capture groups in
\code{\link{alternatives}} will be combined into a single output column).}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}





