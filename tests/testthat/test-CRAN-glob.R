library(nc)
library(testthat)
library(data.table)
source(system.file("test_engines.R", package="nc", mustWork=TRUE), local=TRUE)

db <- system.file("extdata/chip-seq-chunk-db", package="nc", mustWork=TRUE)
glob <- paste0(db, "/*/*/counts/*")
read.bedGraph <- function(f)data.table::fread(
  f, skip=1, col.names = c("chrom","start", "end", "count"))
data.chunk.pattern <- list(
  data="H.*?",
  "/",
  chunk="[0-9]+", as.integer)
test_engines("capture_first_glob returns expected columns", {
  count.dt <- nc::capture_first_glob(glob, data.chunk.pattern, READ=read.bedGraph)
  expect_identical(names(count.dt), c("data", "chunk", "chrom", "start", "end", "count"))
})
