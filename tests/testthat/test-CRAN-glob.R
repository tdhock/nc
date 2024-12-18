library(nc)
library(testthat)
library(data.table)
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

db <- system.file("extdata/chip-seq-chunk-db", package="nc", mustWork=TRUE)
glob <- paste0(db, "/*/*/counts/*")
read.bedGraph <- function(f)data.table::fread(
  f, skip=1, col.names = c("chrom","start", "end", "count"))

if(requireNamespace("R.utils")){

  test_engines("capture_first_glob returns expected columns", {
    count.dt <- nc::capture_first_glob(
      glob,
      data="H.*?",
      "/",
      chunk="[0-9]+", as.integer,
      READ=read.bedGraph)
    computed.cls <- sapply(count.dt, class)
    expected.cls <- c(
      data = "character",
      chunk = "integer",
      chrom = "character",
      start = "integer",
      end = "integer",
      count = "integer")
    expect_identical(computed.cls, expected.cls)
  })

  test_engines("capture_first_glob(type.convert) works", {
    count.dt <- nc::capture_first_glob(
      glob,
      data="H.*?",
      "/",
      chunk="[0-9]+",
      READ=read.bedGraph,
      type.convert=TRUE)
    computed.cls <- sapply(count.dt, class)
    expected.cls <- c(
      data = "character",
      chunk = "integer",
      chrom = "character",
      start = "integer",
      end = "integer",
      count = "integer")
    expect_identical(computed.cls, expected.cls)
  })

}
