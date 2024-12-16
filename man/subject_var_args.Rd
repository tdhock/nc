\name{subject_var_args}
\alias{subject_var_args}
\title{subject var args}
\description{Parse the complete argument list including subject.}
\usage{subject_var_args(..., 
    type.convert = getOption("nc.type.convert", 
        FALSE))}
\arguments{
  \item{\dots}{subject, regex/conversion.}
  \item{type.convert}{If TRUE, use \code{\link[utils]{type.convert}} instead of \code{\link[base]{identity}} for the
default conversion.}
}

\value{Result of \code{\link{var_args_list}} plus subject.}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}





