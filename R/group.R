group <- structure(function # Capture group
### Create a capture group (named column in output). In the vast
### majority of patterns R arguments can/should be used to specify
### names, e.g. list(name=pattern). This is a helper function which is
### useful for programmatically creating group names (see example for
### a typical use case).
(name,
### Column name in output.
  ...
### Regex pattern(s).
){
  if(!(is.character(name) && length(name)==1))stop(
    "first argument of group must be a character string (group name)")
  pattern <- list(...)
  structure(list(pattern), names=name)
### Named list.
}, ex=function(){

  ## Data downloaded from
  ## https://en.wikipedia.org/wiki/Hindu%E2%80%93Arabic_numeral_system
  numerals <- system.file(
    "extdata", "Hindu-Arabic-numerals.txt.gz", package="nc")

  ## Use engine="ICU" for unicode character classes
  ## http://userguide.icu-project.org/strings/regexp e.g. match any
  ## character with a numeric value of 2 (including japanese etc).
  if(requireNamespace("stringi"))
    nc::capture_all_str(
    numerals,
    " ",
    two="[\\p{numeric_value=2}]",
    " ",
    engine="ICU")

  ## Create a table of numerals with script names.
  digits.pattern <- list()
  for(digit in 0:9){
    digits.pattern[[length(digits.pattern)+1]] <- list(
      "[|]",
      nc::group(paste(digit), "[^{|]+"),
      "[|]")
  }
  nc::capture_all_str(
    numerals,
    "\n",
    digits.pattern,
    "[|]",
    " *",
    "\\[\\[",
    name="[^\\]|]+")

})
