\name{capture_longer_spec}
\alias{capture_longer_spec}
\title{capture longer spec}
\description{Create a spec \code{data} table for input to \code{\link[tidyr]{pivot_longer_spec}}.}
\usage{capture_longer_spec(data, 
    ..., values_to = "value")}
\arguments{
  \item{data}{Data table to reshape (actually the \code{data} are ignored, and only the
column names are used).}
  \item{\dots}{Regex and conversion as described in \code{\link{capture_first_vec}}. This is
processed by \code{\link{measure}} so if "column" is used as an argument name
then there will be multiple output columns in the reshaped \code{data}
(analogous to names_to=".value" in \code{\link[tidyr]{pivot_longer}}).}
  \item{values_to}{string to use for name of value column in output (only used if
there is a single output column in the reshaped \code{data}).}
}

\value{\code{data} table describing a reshape longer operation.}

\author{Toby Dylan Hocking}




\examples{

(one.iris <- iris[1,])
(single.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", dim=".*", values_to="cm"))
(multiple.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", column=".*"))
if(requireNamespace("tidyr")){
  tidyr::pivot_longer_spec(one.iris, single.spec)
  tidyr::pivot_longer_spec(one.iris, multiple.spec)
}

}
