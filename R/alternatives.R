alternatives <- function(...){
  in.list <- list(...)
  stopifnot(1 < length(in.list))
  out.list <- in.list[1]
  for(i in 2:length(in.list)){
    out.list[[length(out.list)+1L]] <- "|"
    out.list[[length(out.list)+1L]] <- in.list[[i]]
  }
  out.list
}
