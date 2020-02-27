apply_type_funs <- function
### Convert columns of match.mat using corresponding functions from
### type.list.
(match.mat,
### character matrix (matches X groups).
  type.list
### named list of functions to apply to captured groups.
){
  stopifnot(is.character(match.mat))
  stopifnot(is.matrix(match.mat))
  colnames(match.mat) <- names(type.list)
  dt <- data.table(match.mat)
  for(col.name in names(type.list)){
    type.fun <- type.list[[col.name]]
    tryCatch({
      fun.result <- type.fun(match.mat[, col.name])
    }, error=function(e){
      stop(
        "type conversion functions should take one argument ",
        "(character vector of captured text) and return ",
        "an atomic vector of the same size; ",
        "function for group ",
        col.name,
        " raised an error: ", e$message)
    })
    if(!is.atomic(fun.result)){
      stop(
        "type conversion function for group ",
        col.name,
        " must return atomic vector")
    }
    if(length(fun.result) != nrow(match.mat)){
      stop(
        "type conversion function for group ",
        col.name,
        " returned vector of length ",
        length(fun.result),
        " but expected length ",
        nrow(match.mat))
    }
    set(dt, j=col.name, value=fun.result)
  }
  dt
### data.table with columns defined by calling the functions in
### type.list on the corresponding column of match.mat. 
}

