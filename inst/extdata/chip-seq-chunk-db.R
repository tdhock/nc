bg.vec <- c("H3K4me3_XJ_immune/2/counts/McGill0024.bedGraph.gz","H3K4me3_TDH_immune/9/counts/McGill0024.bedGraph.gz","H3K36me3_AM_immune/9/counts/McGill0101.bedGraph.gz","H3K36me3_TDH_other/1/counts/McGill0019.bedGraph.gz")
prefix <- "https://rcdata.nau.edu/genomic-ml/chip-seq-chunk-db/"
for(suffix in bg.vec){
  local.dir <- "~/R/nc/inst/extdata/chip-seq-chunk-db"
  local.file <- file.path(local.dir, suffix)
  if(!file.exists(local.file)){
    dir.create(dirname(local.file), showWarnings = FALSE, recursive = TRUE)
    u <- paste0(prefix, suffix)
    download.file(u, local.file)
  }
}
