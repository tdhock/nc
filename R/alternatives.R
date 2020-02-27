alternatives <- function
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
}
