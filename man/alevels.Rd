\name{alevels}
\alias{alevels}
\title{Alternative levels}
\description{Create a pattern and a conversion function based on alternative
string literals.}
\usage{alevels(...)}
\arguments{
  \item{\dots}{Optional names are string literals to match; values are
corresponding factor levels.}
}

\value{List of pattern and conversion function that returns factor.}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

## Example 0: melt iris data with literal alternatives -> chr columns.
ichr <- nc::capture_melt_single(
  iris[1,],
  part="Sepal|Petal",
  "[.]",
  dim="Length|Width")
factor(ichr$part)#default factor levels are alphabetical.

## Example 1: melt iris data with alevels() -> factor columns.
(ifac <- nc::capture_melt_single(
  iris[1,],
  part=nc::alevels("Sepal","Petal"),
  "[.]",
  dim=nc::alevels("Length","Width")))
ifac$part #factor with levels in same order as given in alevels().

## Example 2: alevels(literals_to_match="levels_to_use_in_output").
tv_wide <- data.frame(
  id=0,
  train.classif.logloss = 1, train.classif.ce = 2, 
  valid.classif.logloss = 3, valid.classif.ce = 4)
nc::capture_melt_single(
  tv_wide, 
  set=nc::alevels(valid="validation", train="subtrain"),
  "[.]classif[.]",
  measure=nc::alevels(ce="error_prop", auc="AUC", "logloss"))

## Example 3: additional groups which output character columns.
nc::capture_melt_single(
  tv_wide, 
  set_chr=list(set_fac=nc::alevels(valid="validation", train="subtrain")),
  "[.]classif[.]",
  measure_chr=list(measure_fac=nc::alevels(ce="error_prop", auc="AUC", "logloss")))

}
