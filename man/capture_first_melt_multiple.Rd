\name{capture_first_melt_multiple}
\alias{capture_first_melt_multiple}
\title{Capture and melt multiple columns of different types}
\description{Match a regex to \code{subject.df} column names,
then melt the matching columns to multiple
result columns in a tall data table.
It is for the common case of melting several
columns of different types in a "wide" input data table
with regular names.
For melting into a single result column,
see \code{\link{capture_first_melt}}.}
\usage{capture_first_melt_multiple(subject.df, ..., id.vars = NULL, 
    na.rm = FALSE, verbose = getOption("datatable.verbose"))}
\arguments{
  \item{subject.df}{The data.frame with column name subjects.}
  \item{\dots}{Pattern passed to \code{\link{capture_first_vec}} for matching input column
names. There must be an argument named "column" -- each unique
value captured in this \code{\link{group}} becomes a column name in the
output. There must also be at least one other named argument, and
the output will contain a column for each other such named
argument -- see examples. Specifying the regex and output column
names using this syntax can be less repetitive than using
\code{\link[data.table]{patterns}}.}
  \item{id.vars}{columns to use as \code{id.vars} in \code{\link[data.table]{melt.data.table}}. Default
NULL means to use all variables not matched by the pattern.}
  \item{na.rm}{remove missing values from melted data? (passed to
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
  
  ## Example 0: melt iris columns to compare Sepal and Petal dims.
  iris.part.cols <- nc::capture_first_melt_multiple(
    iris,
    column=".*?",
    "[.]",
    dim=".*")
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(dim ~ Species)+
      coord_equal()+
      geom_abline(slope=1, intercept=0, color="grey")+
      geom_point(aes(
        Petal, Sepal),
        data=iris.part.cols)
  }
  
  ## Example 1: melting a wider iris data back to original.
  library(data.table)
  iris.dt <- data.table(
    i=1:nrow(iris),
    iris[,1:4],
    Species=paste(iris$Species))
  print(iris.dt)
  
  ## what if we had two observations on each row?
  set.seed(1)
  iris.rand <- iris.dt[sample(.N)]
  iris.wide <- cbind(treatment=iris.rand[1:75], control=iris.rand[76:150])
  print(iris.wide, topn=2, nrows=10)
  
  ## This is the usual data.table syntax for getting the original iris back.
  iris.melted <- melt(iris.wide, value.factor=TRUE, measure.vars = patterns(
    i="i$",
    Sepal.Length="Sepal.Length$",
    Sepal.Width="Sepal.Width$",
    Petal.Length="Petal.Length$",
    Petal.Width="Petal.Width$",
    Species="Species$"))
  identical(iris.melted[order(i), names(iris.dt), with=FALSE], iris.dt)
  
  ## nc can do the same thing -- you must define an R argument named
  ## column, and another named argument which identifies each group.
  (nc.melted <- nc::capture_first_melt_multiple(
     iris.wide,
     group="[^.]+",
     "[.]",
     column=".*"))
  identical(nc.melted[order(i), names(iris.dt), with=FALSE], iris.dt)
  
  ## This is how we do it using stats::reshape.
  iris.wide.df <- data.frame(iris.wide)
  names(iris.wide.df) <- sub("(.*?)[.](.*)", "\\\\2_\\\\1", names(iris.wide))
  iris.reshaped <- stats::reshape(
    iris.wide.df,
    direction="long",
    timevar="group",
    varying=names(iris.wide.df),
    sep="_")
  identical(data.table(iris.reshaped[, names(iris.dt)])[order(i)], iris.dt)
  
  ## get the parts columns and groups -- is there any difference
  ## between groups? of course not!
  parts.wide <- nc::capture_first_melt_multiple(
    iris.wide,
    group=".*?",
    "[.]",
    column=".*?",
    "[.]",
    dim=".*")
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(dim ~ group)+
      coord_equal()+
      geom_abline(slope=1, intercept=0, color="grey")+
      geom_point(aes(
        Petal, Sepal),
        data=parts.wide)
  }
  
  ## Example 2. Lots of column types, from example(melt.data.table).
  DT <- data.table(
    i_1 = c(1:5, NA),
    i_2 = c(NA,6:10),
    f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
    f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
    c_1 = sample(c(letters[1:3], NA), 6, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
  ## add a couple of list cols
  DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
  DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]
  
  ## original DT syntax is quite repetitive.
  melt(DT, measure=patterns(
             i="^i",
             f="^f",
             d="^d",
             l="^l"
           ))
  
  ## nc syntax uses a single regex rather than four.
  nc::capture_first_melt_multiple(
    DT,
    column="^[^c]",
    "_",
    number="[12]")
  
  ## id.vars can be specified using original DT syntax.
  melt(DT, id=1:2, measure=patterns(
                     f="^f",
                     l="^l"
                   ))
  
  ## id.vars can also be specified using nc syntax.
  nc::capture_first_melt_multiple(
    DT,
    column="^[fl]",
    "_",
    number="[12]",
    id.vars=1:2)
  
  ## reshape does not support list columns.
  reshape(
    DT,
    varying=grep("^[fid]", names(DT)),
    sep="_",
    direction="long",
    timevar="number")
  
  ## Example 3, three children, one family per row, from data.table
  ## vignette.
  family.dt <- fread(text="
family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
  (children.melt <- melt(family.dt, measure = patterns(
                                      dob="^dob", gender="^gender"
                                    ), variable.name="child", na.rm=TRUE, variable.factor=FALSE))
  
  ## nc::field can be used to define group name and pattern at the
  ## same time, to avoid repetitive code.
  (children.nc <- nc::capture_first_melt_multiple(
     family.dt,
     column="[^_]+",
     "_",
     nc::field("child", "", "[1-3]"),
     na.rm=TRUE))
  identical(children.nc[, names(children.melt), with=FALSE], children.melt)
  
  ## reshape works too.
  reshape(
    family.dt,
    varying=grep("child", names(family.dt)),
    direction="long",
    sep="_",
    timevar="child.str")
  
}
