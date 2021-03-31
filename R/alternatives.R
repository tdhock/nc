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

})

altlist <- structure(function
### Create a named list containing named patterns.
(...
### Named patterns.
){
  L <- list(...)
  if(is.null(names(L)) || any(names(L)=="")){
    stop("all arguments to altlist must be named")
  }
  for(i in seq_along(L)){
    L[[i]] <- structure(list(L[[i]]), names=names(L)[[i]])
  }
  L
### Named list to be used with functions base::with and
### nc::alternatives, see examples.
}, ex=function(){

  ## Example 1: matching dates in different formats, but always same
  ## type in each alternative.
  subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
  ## One way to do it would be with alternative format strings.
  idate.mat <- sapply(
    c("%b %d, %Y", "%d %b %Y"),
    function(f)data.table::as.IDate(subject.vec, format=f))
  col.vec <- apply(!is.na(idate.mat), 1, which)
  i.mat <- cbind(1:nrow(idate.mat), col.vec)
  data.table::as.IDate(idate.mat[i.mat])
  ## Another way is with alternative regular expressions.
  pat.list <- nc::altlist(month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}")
  pattern <- with(pat.list, nc::alternatives(
    list(month, " ", day, ", ", year),
    list(day, " ", month, " ", year)))
  match.dt <- nc::capture_first_vec(subject.vec, pattern)
  print(match.dt, class=TRUE)
  match.dt[, data.table::as.IDate(paste0(month, day, year), format="%b %d %Y")]

  ## Example 2: matching dates in different formats, but with
  ## different types in different alternatives.
  subject.vec <- c("3/17/1983", "26 sep 2017")
  month2int <- c(
    jan=1L, feb=2L, mar=3L, apr=4L,  may=5L,  jun=6L,
    jul=7L, aug=8L, sep=9L, oct=10L, nov=11L, dec=12L)
  pat.list <- nc::altlist(
    day=list("[0-9]{2}", as.integer),
    year=list("[0-9]{4}", as.integer))
  pattern <- with(pat.list, nc::alternatives(
    list(month="[0-9]", as.integer, "/", day, "/", year),
    list(day, " ", month="[a-z]{3}", function(m)month2int[m], " ", year)))
  match.dt <- nc::capture_first_vec(subject.vec, pattern)
  print(match.dt, class=TRUE)

  ## Example 3: three alternatives with four groups each.
  subject.vec <- c(
    "EUR 5.00 Theft in delivery inserted in wire transfer 11/02/2021",
    "EUR 50.00 - Refund for theft in delivery - 30/07/2020",
    "EUR68.50 - Refund for theft in delivery 02/07/2020",
    "45.00 EUR 29/10/2020 Refund for theft in delivery",
    "53.00\u20ac Refund for theft in delivery 24/09/2020")
  pat.list <- nc::altlist(
    currency="EUR|\u20ac",
    amount=list("[0-9.]+", as.numeric),
    reason="[A-Za-z ]+?",
    date=list(
      "[0-9]{2}/[0-9]{2}/[0-9]{4}",
      function(d)data.table::as.IDate(d, format="%d/%m/%Y")))
  sep <- function(...){
    pats <- list(...)
    sep.pats <- lapply(seq_along(pats), function(i){
      p <- pats[[i]]
      if(i==1)list(p) else list(list(" - | |"), p)
    })
    list("^", sep.pats, "$")
  }
  pattern <- with(pat.list, nc::alternatives(
    sep(currency, amount, reason, date),
    sep(amount, currency, date, reason),
    sep(amount, currency, reason, date)))
  match.dt <- nc::capture_first_vec(subject.vec, pattern)
  print(match.dt, class=TRUE)

})
