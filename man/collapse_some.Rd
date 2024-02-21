\name{collapse_some}
\alias{collapse_some}
\title{collapse some}
\description{Create character string with some or all items.}
\usage{collapse_some(all.vec, 
    max.first.last = 5, 
    collapse = ",")}
\arguments{
  \item{all.vec}{Vector of all items.}
  \item{max.first.last}{Max number of items to show.}
  \item{collapse}{Passed to paste.}
}

\value{Character string formed by paste with \code{collapse} on some items of
\code{all.vec} (first/last few items used if length is greater than
max.first.last*2, otherwise all items).}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}





