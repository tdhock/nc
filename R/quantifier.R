quantifier <- structure(function
### Create a group with a quantifier.
(...
### Pattern(s) to be enclosed in a group, and a quantifier (last
### argument). A quantifier is character string: "?" for zero or one,
### "*?"  for non-greedy zero or more, "+" for greedy one or more,
### etc.
){
  L <- list(...)
  N <- length(L)
  if(N < 2)stop(
    "quantifier needs at least two arguments (patterns, quantifier)")
  no.names <- is.null(names(L))
  no.last.name <- identical(names(L)[[N]], "")
  has.last.name <- !(no.names||no.last.name)
  if(has.last.name)stop(
    "last argument to quantifier must be un-named")
  last.arg <- L[[N]]
  if(!(is.character(last.arg) && length(last.arg)==1))stop(
    "last argument to quantifier must be character string ",
    "(quantifier such as ?, *, or {0,2})")
  list(L[-N], last.arg)
### A pattern list.
}, ex=function(){

  ## nc::quantifier shouldn't be used when the pattern to be
  ## quantified is just a string literal.
  digits <- "[0-9]+"

  ## nc::quantifier is useful when there is a sequence of patterns to
  ## be quantified, here an optional group with a dash (not captured)
  ## followed by some digits (captured in the chromEnd group).
  str(optional.end <- nc::quantifier("-", chromEnd=digits, "?"))

  ## Use it as a sub-pattern for capturing genomic coordinates.
  chr.pos.vec <- c(
    "chr10:213054000-213055000",
    "chrM:111000",
    "chr1:110-111 chr2:220-222") # two possible matches.
  nc::capture_first_vec(
    chr.pos.vec,
    chrom="chr.*?",
    ":",
    chromStart=digits,
    optional.end)

  ## Another example which uses quantifier twice, for extracting code
  ## chunks from Rmd files.
  vignette.Rmd <- system.file(
    "extdata", "vignette.Rmd", package="nc")
  non.greedy.lines <- nc::quantifier(".*\n", "*?")
  optional.name <- nc::quantifier(" ", name="[^,}]+", "?")
  Rmd.dt <- nc::capture_all_str(
    vignette.Rmd,
    before=non.greedy.lines,
    "```\\{r",
    optional.name,
    parameters=".*",
    "\\}\n",
    code=non.greedy.lines,
    "```")
  Rmd.dt[, chunk := 1:.N]
  Rmd.dt[, .(chunk, name, parameters, some.code=substr(code, 1, 20))]

})
