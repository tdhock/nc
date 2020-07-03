\name{capture_all_str}
\alias{capture_all_str}
\title{Capture all matches in a single subject string}
\description{Capture each match of a regex pattern from one multi-line subject
string or text file. It can be used to convert any regular text
file (web page, log, etc) to a data table, see examples.}
\usage{capture_all_str(..., 
    engine = getOption("nc.engine", 
        "PCRE"), collapse = "\\n")}
\arguments{
  \item{\dots}{subject, name1=pattern1, fun1, etc. The first argument must be a
subject character vector (or file name which is read via
\code{\link[base]{readLines}} to get a subject). After removing missing values,
we use \code{\link[base]{paste}} to \code{collapse} the subject (by default using newline)
and treat it as single character string to search. Arguments after
the first specify the regex/conversion and must be
character/function/list. All character strings are pasted together
to obtain the final regex used for matching. Each string with a
named argument in R becomes a capture \code{\link{group}} in the regex, and the
name is used for the corresponding column of the output data
table. Each named pattern may be followed by at most one function
which is used to convert the values captured by that
pattern. Lists are parsed recursively for convenience.}
  \item{engine}{character string, one of PCRE, ICU, RE2}
  \item{collapse}{separator string for combining elements of subject into a single
string, used as \code{collapse} argument of \code{\link[base]{paste}}.}
}

\value{data.table with one row for each match, and one column for each
capture \code{\link{group}}.}

\author{Toby Dylan Hocking}




\examples{

chr.pos.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000-222,000",
  "this will not match",
  NA, # neither will this.
  "chr1:110-111 chr2:220-222") # two possible matches.
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
## By default elements of subject are treated as separate lines (and
## NAs are removed). Named arguments are used to create capture
## groups, and conversion functions such as keep.digits are used to
## convert the previously named group.
int.pattern <- list("[0-9,]+", keep.digits)
(match.dt <- nc::capture_all_str(
  chr.pos.vec,
  chrom="chr.*?",
  ":",
  chromStart=int.pattern,
  "-",
  chromEnd=int.pattern))
str(match.dt)

## Extract all fields from each alignment block, using two regex
## patterns, then dcast.
info.txt.gz <- system.file(
  "extdata", "SweeD_Info.txt.gz", package="nc")
info.vec <- readLines(info.txt.gz)
info.vec[24:40]
info.dt <- nc::capture_all_str(
  sub("Alignment ", "//", info.vec),
  "//",
  alignment="[0-9]+",
  fields="[^/]+")
(fields.dt <- info.dt[, nc::capture_all_str(
  fields,
  "\\t+",
  variable="[^:]+",
  ":\\t*",
  value=".*"),
  by=alignment])
(fields.wide <- data.table::dcast(fields.dt, alignment ~ variable))

## Capture all csv tables in report -- the file name can be given as
## the subject to nc::capture_all_str, which calls readLines to get
## data to parse.
(report.txt.gz <- system.file(
  "extdata", "SweeD_Report.txt.gz", package="nc"))
(report.dt <- nc::capture_all_str(
  report.txt.gz,
  "//",
  alignment="[0-9]+",
  "\\n",
  csv="[^/]+"
)[, {
  data.table::fread(text=csv)
}, by=alignment])

## Join report with info fields.
report.dt[fields.wide, on=.(alignment)]

## parsing nbib citation file.
(pmc.nbib <- system.file(
  "extdata", "PMC3045577.nbib", package="nc"))
blank <- "\\n      "
pmc.dt <- nc::capture_all_str(
  pmc.nbib,
  Abbreviation="[A-Z]+",
  " *- ",
  value=list(
    ".*",
    list(blank, ".*"), "*"),
  function(x)sub(blank, "", x))
str(pmc.dt)

## What do the variable fields mean? It is explained on
## https://www.nlm.nih.gov/bsd/mms/medlineelements.html which has a
## local copy in this package (downloaded 18 Sep 2019).
fields.html <- system.file(
  "extdata", "MEDLINE_Fields.html", package="nc")
if(interactive())browseURL(fields.html)
fields.vec <- readLines(fields.html)

## It is pretty easy to capture fields and abbreviations if gsub
## used to remove some tags first.
no.strong <- gsub("</?strong>", "", fields.vec)
no.comments <- gsub("<!--.*?-->", "", no.strong)
## grep then capture_first_vec can be used if each desired row in
## the output comes from a single line of the input file.
(h3.vec <- grep("<h3", no.comments, value=TRUE))
h3.pattern <- list(
  nc::field("name", '="', '[^"]+'),
  '"></a>',
  fields.abbrevs="[^<]+")
first.fields.dt <- nc::capture_first_vec(
  h3.vec, h3.pattern)
field.abbrev.pattern <- list(
  Field=".*?",
  " \\\\(",
  Abbreviation="[^)]+",
  "\\\\)",
  "(?: and |$)?")
(first.each.field <- first.fields.dt[, nc::capture_all_str(
  fields.abbrevs, field.abbrev.pattern),
  by=fields.abbrevs])

## If we want to capture the information after the initial h3 line
## of the input, e.g. the rest column below which contains a
## description/example for each field, then capture_all_str can be
## used on the full input file.
h3.fields.dt <- nc::capture_all_str(
  no.comments,
  h3.pattern,
  '</h3>\\n',
  rest="(?:.*\\n)+?", #exercise: get the examples.
  "<hr />\\n")
(h3.each.field <- h3.fields.dt[, nc::capture_all_str(
  fields.abbrevs, field.abbrev.pattern),
  by=fields.abbrevs])

## Either method of capturing abbreviations gives the same result.
identical(first.each.field, h3.each.field)

## but the capture_all_str method returns the additional rest column
## which contains data after the initial h3 line.
names(first.fields.dt)
names(h3.fields.dt)
cat(h3.fields.dt[fields.abbrevs=="Volume (VI)", rest])

## There are 66 Field rows across three tables.
a.href <- list('<a href=[^>]+>')
(td.vec <- fields.vec[240:280])
fields.pattern <- list(
  "<td.*?>",
  a.href,
  Fields="[^()<]+",
  "</a></td>\\n")
(td.only.Fields <- nc::capture_all_str(fields.vec, fields.pattern))

## Extract Fields and Abbreviations. Careful: most fields have one
## abbreviation, but one field has none, and two fields have two.
(td.fields.dt <- nc::capture_all_str(
  fields.vec,
  fields.pattern,
  "<td[^>]*>",
  "(?:\\n<div>)?",
  a.href, "?",
  abbrevs=".*?",
  "</"))

## Get each individual abbreviation from the previously captured td
## data.
td.each.field <- td.fields.dt[, {
  f <- nc::capture_all_str(
    Fields,
    Field=".*?",
    "(?:$| and )")
  a <- nc::capture_all_str(
    abbrevs,
    "\\\\(",
    Abbreviation="[^)]+",
    "\\\\)")
  if(nrow(a)==0)list() else cbind(f, a)
}, by=Fields]
str(td.each.field)
td.each.field[td.fields.dt, .(
  count=.N
), on=.(Fields), by=.EACHI][order(count)]

## There is a typo in the data captured from the h3 headings.
td.each.field[!Field \%in\% h3.each.field$Field]
h3.each.field[!Field \%in\% td.each.field$Field]

## Abbreviations are consistent.
td.each.field[!Abbreviation \%in\% h3.each.field$Abbreviation]
h3.each.field[!Abbreviation \%in\% td.each.field$Abbreviation]

## There is a a table that provides a description of each comment
## type.
(comment.vec <- fields.vec[840:860])
comment.dt <- nc::capture_all_str(
  fields.vec,
  "<td><strong>",
  Field="[^<]+",
  "</strong></td>\\n",
  "<td><strong>\\\\(",
  Abbreviation="[^)]+",
  "\\\\)</strong></td>\\n",
  "<td>",
  description=".*",
  "</td>\\n")
str(comment.dt)

## Join to original PMC citation file in order to see what the
## abbreviations used in that file mean.
all.abbrevs <- rbind(
  td.each.field[, .(Field, Abbreviation)],
  comment.dt[, .(Field, Abbreviation)])
all.abbrevs[pmc.dt, .(
  Abbreviation,
  Field,
  value=substr(value, 1, 20)
), on=.(Abbreviation)]

## There is a listing of examples for each comment type.
(comment.ex.dt <- nc::capture_all_str(
  fields.vec[938],
  "br />\\\\s*",
  Abbreviation="[A-Z]+",
  "\\\\s*-\\\\s*",
  citation="[^<]+?",
  list(
    "[.] ",
    nc::field("PMID", ": ", "[0-9]+")
  ), "?",
  "<"))

## Join abbreviations to see what kind of comments.
all.abbrevs[comment.ex.dt, on=.(Abbreviation)]

## parsing bibtex file.
refs.bib <- system.file(
  "extdata", "namedCapture-refs.bib", package="nc")
refs.vec <- readLines(refs.bib)
at.lines <- grep("@", refs.vec, value=TRUE)
str(at.lines)
refs.dt <- nc::capture_all_str(
  refs.vec,
  "@",
  type="[^{]+",
  "[{]",
  ref="[^,]+",
  ",\\n",
  fields="(?:.*\\n)+?.*",
  "[}]\\\\s*(?:$|\\n)")
str(refs.dt)

## parsing each field of each entry.
eq.lines <- grep("=", refs.vec, value=TRUE)
str(eq.lines)
strip <- function(x)sub("^\\\\s*\\\\{*", "", sub("\\\\}*,?$", "", x))
refs.fields <- refs.dt[, nc::capture_all_str(
  fields,
  "\\\\s+",
  variable="\\\\S+",
  "\\\\s+=",
  value=".*", strip),
  by=.(type, ref)]
str(refs.fields)
with(refs.fields[ref=="HockingUseR2011"], structure(
  as.list(value), names=variable))
## the URL of my talk is now
## https://user2011.r-project.org/TalkSlides/Lightening/2-StatisticsAndProg_3-Hocking.pdf

if(!grepl("solaris", R.version$platform)){#To avoid CRAN check error on solaris
  ## Parsing wikimedia tables: each begins with {| and ends with |}.
  emoji.txt.gz <- system.file(
    "extdata", "wikipedia-emoji-text.txt.gz", package="nc")
  tables <- nc::capture_all_str(
    emoji.txt.gz,
    "\\n[{][|]",
    first=".*",
    '\\n[|][+] style="',
    nc::field("font-size", ":", '.*?'),
    '" [|] ',
    title=".*",
    lines="(?:\\n.*)*?",
    "\\n[|][}]")
  str(tables)
  ## Rows are separated by |-
  rows.dt <- tables[, {
    row.vec <- strsplit(lines, "|-", fixed=TRUE)[[1]][-1]
    .(row.i=seq_along(row.vec), row=row.vec)
  }, by=title]
  str(rows.dt)
  ## Try to parse columns from each row. Doesn't work for second table
  ## https://en.wikipedia.org/w/index.php?title=Emoji&oldid=920745513#Skin_color
  ## because some entries have rowspan=2.
  contents.dt <- rows.dt[, nc::capture_all_str(
    row,
    "[|] ",
    content=".*?",
    "(?: [|]|\\n|$)"),
    by=.(title, row.i)]
  contents.dt[, .(cols=.N), by=.(title, row.i)]
  ## Make data table from
  ## https://en.wikipedia.org/w/index.php?title=Emoji&oldid=920745513#Emoji_versus_text_presentation
  contents.dt[, col.i := 1:.N, by=.(title, row.i)]
  data.table::dcast(
    contents.dt[title=="Sample emoji variation sequences"],
    row.i ~ col.i,
    value.var="content")
}

## Simple way to extract code chunks from Rmd.
vignette.Rmd <- system.file(
  "extdata", "vignette.Rmd", package="nc")
non.greedy.lines <- list(
  list(".*\\n"), "*?")
optional.name <- list(
  list(" ", name="[^,}]+"), "?")
Rmd.dt <- nc::capture_all_str(
  vignette.Rmd,
  before=non.greedy.lines,
  "```\\\\{r",
  optional.name,
  parameters=".*",
  "\\\\}\\n",
  code=non.greedy.lines,
  "```")
Rmd.dt[, chunk := 1:.N]
Rmd.dt[, .(chunk, name, parameters, some.code=substr(code, 1, 20))]

## Extract individual parameter names and values.
Rmd.dt[, nc::capture_all_str(
  parameters,
  ", *",
  variable="[^= ]+",
  " *= *",
  value="[^ ,]+"),
  by=chunk]

## Simple way to extract code chunks from Rnw.
vignette.Rnw <- system.file(
  "extdata", "vignette.Rnw", package="nc")
Rnw.dt <- nc::capture_all_str(
  vignette.Rnw,
  before=non.greedy.lines,
  "<<",
  name="[^,>]*",
  parameters=".*",
  ">>=\\n",
  code=non.greedy.lines,
  "@")
Rnw.dt[, .(name, parameters, some.code=substr(code, 1, 20))]

## The next example involves timing some compression programs that
## were run on a 159 megabyte input/uncompressed text file. Here is
## how to get a data table from the time command line output.
times.out <- system.file(
  "extdata", "compress-times.out", package="nc", mustWork=TRUE)
times.dt <- nc::capture_all_str(
  times.out,
  "coverage.bedGraph ",
  program=".*?",
  " coverage.bedGraph.",
  suffix=".*",
  "\\n\\nreal\\t",
  minutes.only="[0-9]+", as.numeric,
  "m",
  seconds.only="[0-9.]+", as.numeric)
times.dt[, seconds := minutes.only*60+seconds.only]
times.dt

## join with output from du command line program.
sizes.out <- system.file(
  "extdata", "compress-sizes.out", package="nc", mustWork=TRUE)
sizes.dt <- data.table::fread(
  file=sizes.out,
  col.names=c("megabytes", "file"))
sizes.dt[, suffix := sub("coverage.bedGraph.?", "", file)]
join.dt <- times.dt[sizes.dt, on="suffix"][order(megabytes)]
join.dt[file=="coverage.bedGraph", seconds := 0]
join.dt

## visualize with ggplot2.
if(require(ggplot2)){
  ggplot(join.dt, aes(
    seconds, megabytes, label=suffix))+
    geom_text(vjust=-0.5)+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()
}

}
