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
  list(L[-N], L[[N]])
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
  
})
