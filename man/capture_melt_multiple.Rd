\name{capture_melt_multiple}
\alias{capture_melt_multiple}
\title{Capture and melt into multiple columns}
\description{Attempt to match a regex to \code{subject.df} column names,
then melt the matching columns to multiple
result columns in a tall data table.
It is for the common case of melting four or more
columns of different types in a "wide" input data table
with regular names.
For melting into a single result column,
see \code{\link{capture_melt_single}}.}
\usage{capture_melt_multiple(subject.df, 
    ..., na.rm = FALSE, 
    verbose = getOption("datatable.verbose"))}
\arguments{
  \item{subject.df}{The data.frame with column name subjects.}
  \item{\dots}{Pattern/engine passed to \code{\link{capture_first_vec}} along with
nomatch.error=FALSE, for matching input column names to
reshape. There must be a \code{\link{group}} named "column" -- each unique value
captured in this \code{\link{group}} becomes a reshape column name in the
output. There must also be at least one other \code{\link{group}}, and the
output will contain a column for each other \code{\link{group}} -- see
examples. Specifying the regex and output column names using this
syntax can be less repetitive than using \code{\link[data.table]{patterns}}.}
  \item{na.rm}{Remove missing values from melted data? (passed to
\code{\link[data.table]{melt.data.table}})}
  \item{verbose}{Print \code{verbose} output messages? (passed to
\code{\link[data.table]{melt.data.table}})}
}

\value{Data table of melted/tall data, with a new column for each unique
value of the capture \code{\link{group}} named "column", and a new column for
each other capture \code{\link{group}}.}

\author{Toby Dylan Hocking}



\seealso{Internally we call data.table::melt.data.table with
value.name=a character vector of unique values
of the column capture group, and
measure.vars=a list of corresponding column indices.}
\examples{

  ## Example 1: melt iris columns to compare Sepal and Petal dims, as
  ## in cdata package, https://winvector.github.io/cdata/
  (iris.part.cols <- nc::capture_melt_multiple(
    iris,
    column=".*?",
    "[.]",
    dim=".*"))
  iris.part.cols[Sepal<Petal] #Sepals are never smaller than Petals.
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(dim ~ Species)+
      coord_equal()+
      geom_abline(slope=1, intercept=0, color="grey")+
      geom_point(aes(
        Petal, Sepal),
        shape=1,
        data=iris.part.cols)
  }

  ## Example 2. melt iris to Length and Width columns.
  (iris.dim.cols <- nc::capture_melt_multiple(
    iris,
    part=".*?",
    "[.]",
    column=".*"))
  iris.dim.cols[Length<Width] #Length is never less than Width.

  ## Example 3. Lots of column types, from example(melt.data.table).
  set.seed(1)
  DT <- data.table::data.table(
    i_1 = c(1:5, NA),
    i_2 = c(NA,6:10),
    f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
    f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
    c_1 = sample(c(letters[1:3], NA), 6, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
  ## nc syntax melts to three output columns of different types using
  ## a single regex (na.rm=FALSE by default in order to avoid losing
  ## information).
  nc::capture_melt_multiple(
    DT,
    column="[^c]",
    "_",
    number="[12]")

  ## Example 4, three children, one family per row, from data.table
  ## vignette.
  family.dt <- data.table::fread(text="
family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
  ## nc::field can be used to define group name and pattern at the
  ## same time, to avoid repetitive code.
  (children.nc <- nc::capture_melt_multiple(
    family.dt,
    column=".+",
    "_",
    nc::field("child", "", "[1-3]"),
    na.rm=TRUE))

}
