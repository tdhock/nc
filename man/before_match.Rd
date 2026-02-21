\name{before_match}
\alias{before_match}
\title{before match}
\description{Augment pattern so that it can be used to match an entire string. Useful for complex find and replace operations, when used with \code{\link{capture_all_str}}.}
\usage{before_match(...)}
\arguments{
  \item{\dots}{pattern as in \code{\link{capture_first_vec}}.}
}



\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

markdown_link <- list(
  "\\\\[",
  title=".*?",
  "\\\\]\\\\(",
  url="http.*?",
  "\\\\)")
markdown_subject <- "before [foo](http) between [bar text](http) after\\n"
nc::capture_all_str(markdown_subject, markdown_link)

before_link <- nc::before_match(markdown_link)
(all_dt <- nc::capture_all_str(markdown_subject, before_link))

## before + match = full subject.
identical(all_dt[, paste(paste0(before, match), collapse="")], markdown_subject)

## replace with org link.
all_dt[, paste(paste0(before, ifelse(
  .I==.N, "", sprintf("[[\%s][\%s]]", url, title)
)), collapse="")]

}
