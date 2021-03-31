alternatives <- structure(function
### Make a pattern that matches one of the specified alternatives.
(...
### Each argument is a different alternative pattern.
){
  in.list <- list(...)
  if(length(in.list) < 2){
    stop("alternatives should have at least two arguments")
  }
  out.list <- in.list[1]
  for(i in 2:length(in.list)){
    out.list <- c(out.list, c("|", in.list[i]))
  }
  out.list
### Pattern list.
}, ex=function(){

  ## simple example.
  subject <- c("foooo1", "barrr2")
  str(foo.or.bar <- nc::alternatives(bar="bar+", foo="fo+"))
  str(foo.or.bar <- list(bar="bar+", "|", foo="fo+"))#same
  nc::capture_first_vec(subject, foo.or.bar, number="[12]")

  ## More complicated regular expression for matching the JobID column
  ## of SLURM sacct output.
  JobID <- c(
    "13937810_25", "13937810_25.batch",
    "13937810_25.extern", "14022192_[1-3]", "14022204_[4]")
  int.pattern <- list("[0-9]+", as.integer)
  ## Match the whole range inside square brackets.
  range.pattern <- list(
    "[[]",
    task.start=int.pattern,
    nc::quantifier("-", task.end=int.pattern, "?"),
    "[]]")
  nc::capture_first_vec(JobID, range.pattern, nomatch.error=FALSE)

  ## Match either a single task ID or a range, after an underscore.
  task.pattern <- list(job="[0-9]+", "_", nc::alternatives(
    task.id=int.pattern,
    range.pattern))
  nc::capture_first_vec(JobID, task.pattern)

  ## If the same capture group name is present in more than one
  ## alternative, then only one column of output will be reported for
  ## each unique capture group name.
  subject.vec <- c(
    "EUR 5.00 Theft in delivery inserted in wire transfer 11/02/2021",
    "EUR 50.00 - Refund for theft in delivery - 30/07/2020",
    "EUR68.50 - Refund for theft in delivery 02/07/2020",
    "45.00 EUR 29/10/2020 Refund for theft in delivery",
    "53.00\u20ac Refund for theft in delivery 24/09/2020")
  pattern.list <- list(
    list(currency="EUR|\u20ac"),
    list(amount="[0-9.]+", as.numeric),
    list(reason="[A-Za-z ]+?"),
    list(date="[0-9]{2}/[0-9]{2}/[0-9]{4}"))
  names(pattern.list) <- sapply(pattern.list, function(L)names(L)[[1]])
  sep <- list(" - | |")
  pattern <- function(...){
    pats <- list(...)
    sep.pats <- lapply(seq_along(pats), function(i){
      p <- pats[[i]]
      if(i==1)list(p) else list(sep, p)
    })
    list("^", sep.pats, "$")
  }
  alt.one <- with(pattern.list, pattern(
    currency, amount, reason, date))
  nc::capture_first_vec(subject.vec, alt.one, nomatch.error=FALSE)

  alt.list <- with(pattern.list, nc::alternatives(
    pattern(currency, amount, reason, date),
    pattern(amount, currency, date, reason),
    pattern(amount, currency, reason, date)))
  nc::capture_first_vec(subject.vec, alt.list)

})
