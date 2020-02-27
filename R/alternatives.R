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
  foo.or.bar <- nc::alternatives(bar="bar+", foo="fo+")
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
