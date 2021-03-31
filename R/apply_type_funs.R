apply_type_funs <- function
### Convert columns of match.mat using corresponding functions from
### type.list, then handle any duplicate capture group names.
(match.mat,
### Character matrix (matches X groups).
  type.list
### Named list of functions to apply to captured groups. If there are
### any duplicate names, they must be in alternatives (only one match
### per unique group name, otherwise error).
){
  stopifnot(is.character(match.mat))
  stopifnot(is.matrix(match.mat))
  colnames(match.mat) <- names(type.list)
  dt <- data.table(match.mat)
  for(col.i in seq_along(type.list)){
    type.fun <- type.list[[col.i]]
    if(!identical(type.fun, identity)){
      col.for.err <- paste0(col.i, "(", names(type.list)[[col.i]], ")")
      tryCatch({
        fun.result <- type.fun(match.mat[, col.i])
      }, error=function(e){
        stop(
          "type conversion functions should take one argument ",
          "(character vector of captured text) and return ",
          "an atomic vector of the same size; ",
          "function for group ",
          col.for.err,
          " raised an error: ", e$message)
      })
      if(!is.atomic(fun.result)){
        stop(
          "type conversion function for group ",
          col.for.err,
          " must return atomic vector")
      }
      if(length(fun.result) != nrow(match.mat)){
        stop(
          "type conversion function for group ",
          col.for.err,
          " returned vector of length ",
          length(fun.result),
          " but expected length ",
          nrow(match.mat))
      }
      set(dt, j=col.i, value=fun.result)
    }
  }
  ## handle duplicates (error or delete columns).
  is.match <- match.mat!=""
  name.tab <- table(names(type.list))
  dup.name.vec <- names(name.tab)[1 < name.tab]
  remove.cols <- list()
  for(dup.name in dup.name.vec){
    dup.col.indices <- which(names(type.list)==dup.name)
    is.match.name <- is.match[, dup.col.indices]
    if(!all(rowSums(is.match.name) == 1)){
      stop("duplicate capture group names are only allowed in alternatives, problem: ", dup.name)
    }
    alt.i.vec <- apply(is.match.name, 1, which)
    alt.i.mat <- cbind(1:nrow(match.mat), alt.i.vec)
    conv.mat <- as.matrix(dt[, dup.col.indices, with=FALSE])
    set(dt, j=dup.col.indices[[1]], value=conv.mat[alt.i.mat])
    remove.cols[[dup.name]] <- dup.col.indices[-1]
  }
  if(length(remove.cols))set(dt, j=unlist(remove.cols), value=NULL)
  dt
### data.table with columns defined by calling the functions in
### type.list on the corresponding column of match.mat. Even if
### type.list has duplicated names, the output data.table will have
### unique column names (identically named capture groups in
### alteratives will be combined into a single output column).
}

