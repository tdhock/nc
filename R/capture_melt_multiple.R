capture_melt_multiple <- structure(function # Capture and melt into multiple columns
### Match a regex to column names of a wide data frame (many
### columns/few rows), then melt/reshape the matching columns into
### multiple result columns in a taller/longer data table (fewer
### columns/more rows). Input should be a data frame with four or more
### regularly named columns of possibly different types to reshape,
### and output is a data table with at least two columns of reshaped
### data. For melting into a single result column, see
### capture_melt_single.
(...,
### First argument must be a data frame to melt/reshape; column names
### of this data frame will be used as the subjects for regex
### matching. Other arguments (regex/conversion/engine) are passed to
### capture_first_vec along with nomatch.error=FALSE. The regex must
### define a group named "column" -- each unique value captured in
### this group becomes a column name for the reshaped data in the
### output. There must also be at least one other group, and the
### output will contain a column for each other group -- see
### examples.
  fill=FALSE,
### If TRUE, fill missing input reshape columns with runs of rows with
### missing values in the output reshape columns. Otherwise stop with
### an error (default).
  na.rm=FALSE,
### Remove missing values from melted data? (passed to
### data.table::melt.data.table)
  verbose=getOption("datatable.verbose")
### Print verbose output messages? (passed to
### data.table::melt.data.table)
){
  L <- melt_list(measure_multiple, list(...), fill=fill)
  melt(
    L[["data"]],
    measure.vars=L[["measure.vars"]],
    na.rm=na.rm,
    value.factor=FALSE,
    verbose=verbose)
### Data table of reshaped/melted/tall/long data, with a new column
### for each unique value of the capture group named "column", and a
### new column for each other capture group.
}, ex=function(){

  data.table::setDTthreads(1)

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
    l_2 = list(NULL, NA, c(NA,NA), logical(), 1:2, TRUE),
    d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
    d_2 = as.Date(6:1, origin="2012-01-01"))
  ## nc syntax melts to three output columns of different types using
  ## a single regex (na.rm=FALSE by default in order to avoid losing
  ## information).
  nc::capture_melt_multiple(
    DT,
    column="[dfi]",
    "_",
    number="[12]", as.integer)

  ## fill=TRUE means to output NA in positions that correspond to
  ## missing input columns (in this case, there is no l_1 nor c_2).
  nc::capture_melt_multiple(
    DT,
    column=".*",
    "_",
    number="[12]", as.integer,
    fill=TRUE)

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
    nc::field("child", "", "[1-3]", as.integer), 
    na.rm=TRUE))

  ## Example 5: wide data CSV with 100 possible peaks per row, each
  ## peak has three attributes (Allele, Height, Size) from
  ## https://lftdi.camden.rutgers.edu/repository/PROVEDIt_1-5-Person%20CSVs%20Filtered.zip
  PROVEDIt.csv <- system.file(
    "extdata", "RD12-0002_PP16HS_5sec_GM_F_1P.csv",
    package="nc", mustWork=TRUE)
  PROVEDIt.wide <- data.table::fread(PROVEDIt.csv)
  names(PROVEDIt.wide)
  PROVEDIt.tall <- nc::capture_melt_multiple(
    PROVEDIt.wide,
    column=".*",
    " ",
    peak="[0-9]+", as.integer,
    na.rm=TRUE)
  head(PROVEDIt.tall)

  ## plot number of peaks per row.
  peaks.per.sample.marker <- PROVEDIt.tall[, .(
    peaks=.N
  ), by=.(`Sample File`, Marker)][order(peaks)]
  if(require(ggplot2)){
    ggplot()+
      geom_histogram(aes(
        peaks),
        data=peaks.per.sample.marker,
        binwidth=1)
  }

  ## which row has the most peaks?
  (most <- PROVEDIt.tall[which.max(peak), .(`Sample File`, Marker, Dye)])
  PROVEDIt.tall[most, on=names(most)]
  PROVEDIt.wide[most, on=names(most)]

})

