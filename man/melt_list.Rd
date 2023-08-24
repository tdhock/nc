\name{melt_list}
\alias{melt_list}
\title{melt list}
\description{Compute a list of arguments to pass to
\code{\link[data.table]{melt.data.table}}.}
\usage{melt_list(measure.fun, 
    dot.args, ...)}
\arguments{
  \item{measure.fun}{\code{\link{measure_single}} or \code{\link{measure_multiple}}.}
  \item{dot.args}{list of arguments for \code{\link{check_df_names}}.}
  \item{\dots}{passed to \code{measure.fun}.}
}



\author{Toby Dylan Hocking}





