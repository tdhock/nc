capture_melt_single <- structure(function # Capture and melt into a single column
### Match a regex to column names of a wide data frame (many
### columns/few rows), then melt/reshape the matching columns into a
### single result column in a taller/longer data table (fewer columns/more
### rows). It is for the common case of melting several columns of
### the same type in a "wide" input data table which has several
### distinct pieces of information encoded in each column name. For
### melting into several result columns of possibly different types,
### see capture_melt_multiple.
(...,
### First argument must be a data frame to melt/reshape; column names
### of this data frame will be used as the subjects for regex
### matching. Other arguments (regex/conversion/engine) are passed to
### capture_first_vec along with nomatch.error=FALSE.
  value.name="value",
### Name of the column in output which has values taken from
### melted/reshaped column values of input (passed to
### data.table::melt.data.table).
  na.rm=TRUE,
### remove missing values from melted data? (passed to
### data.table::melt.data.table)
  verbose=getOption("datatable.verbose")
### Print verbose output messages? (passed to
### data.table::melt.data.table)
){
  L <- melt_list(measure_single, list(...), value.name=value.name)
  melt(
    L[["data"]],
    measure.vars=L[["measure.vars"]],
    value.name=value.name,
    na.rm=na.rm,
    value.factor=FALSE,
    verbose=verbose)
### Data table of reshaped/melted/tall/long data, with a new column
### for each named argument in the pattern, and additionally
### variable/value columns.
}, ex=function(){

  data.table::setDTthreads(1)

  ## Example 1: melt iris data and barplot for each numeric variable.
  (iris.tall <- nc::capture_melt_single(
    iris,
    part=".*",
    "[.]",
    dim=".*",
    value.name="cm"))
  ## Histogram of cm for each variable.
  if(require("ggplot2")){
    ggplot()+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(part ~ dim)+
      geom_bar(aes(cm), data=iris.tall)
  }

  ## Example 2: melt who data and use type conversion functions for
  ## year limits (e.g. for censored regression).
  if(requireNamespace("tidyr")){
    data(who, package="tidyr", envir=environment())
    ##2.1 just extract diagnosis and gender to chr columns.
    new.diag.gender <- list(#save pattern as list for re-use later.
      "new_?",
      diagnosis=".*",
      "_",
      gender=".")
    who.tall.chr <- nc::capture_melt_single(who, new.diag.gender, na.rm=TRUE)
    print(head(who.tall.chr))
    str(who.tall.chr)
    ##2.2 also extract ages and convert to numeric output columns.
    who.tall.num <- nc::capture_melt_single(
      who,
      new.diag.gender,#previous pattern for matching diagnosis and gender.
      ages=list(#new pattern for matching age range.
        min.years="0|[0-9]{2}", as.numeric,#in-line type conversion functions.
        max.years="[0-9]{0,2}", function(x)ifelse(x=="", Inf, as.numeric(x))),
      value.name="count",
      na.rm=TRUE)
    print(head(who.tall.num))
    str(who.tall.num)
    ##2.3 compute total count for each age range then display the
    ##subset with max.years lower than a threshold.
    who.age.counts <- who.tall.num[, .(
      total=sum(count)
    ), by=.(min.years, max.years)]
    print(who.age.counts[max.years < 50])
  }

  ## Example 3: pepseq data.
  if(requireNamespace("R.utils")){#for reading gz files with data.table
    pepseq.dt <- data.table::fread(
      system.file("extdata", "pepseq.txt", package="nc", mustWork=TRUE))
    u.pepseq <- pepseq.dt[, unique(names(pepseq.dt)), with=FALSE]
    nc::capture_melt_single(
      u.pepseq,
      "^",
      prefix=".*?",
      nc::field("D", "", ".*?"),
      "[.]",
      middle=".*?",
      "[.]",
      "[0-9]+",
      suffix=".*",
      "$")
  }

})

