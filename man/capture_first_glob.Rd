\name{capture_first_glob}
\alias{capture_first_glob}
\title{capture first glob}
\description{Glob files, then use \code{\link{capture_first_vec}} to get meta-data from each
file name, and combine with contents of each file.}
\usage{capture_first_glob(glob, 
    ..., READ = fread)}
\arguments{
  \item{glob}{string: \code{glob} specifying files to read.}
  \item{\dots}{passed to \code{\link{capture_first_vec}}, should include pattern used on vector
of file names to get meta-data.}
  \item{READ}{function of one argument (file name) which returns a data table,
default \code{\link[data.table]{fread}}.}
}

\value{Data table with columns of meta-data specified by pattern, plus
contents of all files specified by \code{glob}.}

\author{Toby Hocking <toby.hocking@r-project.org> [aut, cre]}




\examples{

data.table::setDTthreads(1)

## Example 0: iris data, one file per species.
library(data.table)
dir.create(iris.dir <- tempfile())
icsv <- function(sp)file.path(iris.dir, paste0(sp, ".csv"))
data.table(iris)[, fwrite(.SD, icsv(Species)), by=Species]
dir(iris.dir)
data.table::fread(file.path(iris.dir,"setosa.csv"), nrows=2)
(iglob <- file.path(iris.dir,"*.csv"))
nc::capture_first_glob(iglob, Species="[^/]+", "[.]csv")

## Example 1: four files, two capture groups, custom read function.
db <- system.file("extdata/chip-seq-chunk-db", package="nc", mustWork=TRUE)
suffix <- if(interactive())"gz" else "head"
(glob <- paste0(db, "/*/*/counts/*", suffix))
Sys.glob(glob)
read.bedGraph <- function(f)data.table::fread(
  f, skip=1, col.names = c("chrom","start", "end", "count"))
data.chunk.pattern <- list(
  data="H.*?",
  "/",
  chunk="[0-9]+", as.integer)
(data.chunk.dt <- nc::capture_first_glob(glob, data.chunk.pattern, READ=read.bedGraph))

## Write same data set in Hive partition, then re-read.
if(requireNamespace("arrow") && arrow::arrow_with_dataset()){
  path <- tempfile()
  max_rows_per_file <- if(interactive())3 else 1000
  arrow::write_dataset(
    dataset=data.chunk.dt,
    path=path,
    format="csv",
    partitioning=c("data","chunk"),
    max_rows_per_file=max_rows_per_file)
  hive.glob <- file.path(path, "*", "*", "*.csv")
  hive.pattern <- list(
    nc::field("data","=",".*?"),
    "/",
    nc::field("chunk","=",".*?", as.integer),
    "/",
    nc::field("part","-","[0-9]+", as.integer))
  hive.dt <- nc::capture_first_glob(hive.glob, hive.pattern)
  hive.dt[, .(rows=.N), by=.(data,chunk,part)]
}

## Example 2: more complex pattern.
count.dt <- nc::capture_first_glob(
  glob,
  data.chunk.pattern,
  "/counts/", 
  name=list("McGill", id="[0-9]+", as.integer),
  READ=read.bedGraph)
count.dt[, .(count=.N), by=.(data, chunk, name, chrom)]

if(require(ggplot2)){
  ggplot()+
    facet_wrap(~data+chunk+name+chrom, labeller=label_both, scales="free")+
    geom_step(aes(
      start, count),
      data=count.dt)
}

## Example 3: parsing non-CSV data.
vignettes <- system.file("extdata/vignettes", package="nc", mustWork=TRUE)
non.greedy.lines <- list(
  list(".*\\n"), "*?")
optional.name <- list(
  list(" ", chunk_name="[^,}]+"), "?")
chunk.pattern <- list(
  before=non.greedy.lines,
  "```\\\\{r",
  optional.name,
  parameters=".*",
  "\\\\}\\n",
  code=non.greedy.lines,
  "```")
chunk.dt <- nc::capture_first_glob(
  paste0(vignettes, "/*.Rmd"),
  "/v",
  vignette_number="[0-9]", as.integer,
  "-",
  vignette_name=".*?",
  ".Rmd",
  READ=function(f)nc::capture_all_str(f, chunk.pattern))
chunk.dt[, chunk_number := seq_along(chunk_name), by=vignette_number]
chunk.dt[, .(
  vignette_number, vignette_name, chunk_number, chunk_name, 
  lines=nchar(code))]
cat(chunk.dt$code[2])
  
}
