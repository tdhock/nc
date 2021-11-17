apply_type_funs <- function
### Convert columns of match.mat using corresponding functions from
### fun.list, then handle any duplicate capture group names.
(match.mat,
### Character matrix (matches X groups).
  fun.list
### Named list of functions to apply to captured groups. If there are
### any duplicate names, they must be in alternatives (only one match
### per unique group name, otherwise error).
){
  stopifnot(is.character(match.mat))
  stopifnot(is.matrix(match.mat))
  colnames(match.mat) <- names(fun.list)
  dt <- data.table(match.mat)
  for(col.i in seq_along(fun.list)){
    type.fun <- fun.list[[col.i]]
    if(!identical(type.fun, identity)){
      col.for.err <- paste0(col.i, "(", names(fun.list)[[col.i]], ")")
      match.vec <- match.mat[, col.i]
      tryCatch({
        fun.result <- type.fun(match.vec)
      }, error=function(e){
        stop(domain=NA, gettextf("type conversion functions should take one argument (character vector of captured text) and return an atomic vector of the same size; function for group %s raised an error: %s", col.for.err, e$message))
      })
      if(!is.atomic(fun.result)){
        stop(domain=NA, gettextf("each type conversion function must return an atomic vector, but function for group %s did not", col.for.err))
      }
      if(length(fun.result) != nrow(match.mat)){
        stop(domain=NA, gettextf("type conversion function for group %s returned vector of length %s but expected length %s", col.for.err, length(fun.result), nrow(match.mat)))
      }
      if(any(is.na(match.vec) & !is.na(fun.result))){
        stop(domain=NA, gettextf("a non-match(NA) was converted to a match(non-NA) by the conversion function in group %s; please fix conversion function", col.for.err))
      }
      set(dt, j=col.i, value=fun.result)
    }
  }
  ## handle duplicates (error or delete columns).
  is.match <- !is.na(match.mat) & match.mat!=""
  dt.type.vec <- sapply(dt, typeof)
  name.tab <- table(names(fun.list))
  dup.name.vec <- names(name.tab)[1 < name.tab]
  remove.col.list <- list()
  for(dup.name in dup.name.vec){
    dup.col.indices <- which(names(fun.list)==dup.name)
    dup.type.tab <- table(dt.type.vec[dup.col.indices])
    if(1 < length(dup.type.tab)){
      stop(domain=NA, gettextf("capture groups with identical names should have conversion functions that all return the same type; problem group name=%s has types %s", dup.name, paste(names(dup.type.tab), collapse = ",")))
    }
    is.match.name <- is.match[, dup.col.indices]
    if(any(1 < rowSums(is.match.name))){
      stop(domain=NA, gettextf("duplicate capture group names are only allowed in alternatives, problem: %s", dup.name))
    }
    alt.i.vec <- as.integer(apply(is.match.name, 1, which))
    orig.i.vec <- dup.col.indices[alt.i.vec]
    ## Columns for alternatives other than the first will be removed.
    remove.col.vec <- dup.col.indices[-1]
    for(remove.col in remove.col.vec){
      remove.i <- which(remove.col == orig.i.vec)
      ## Copy values captured in this alternative to the first
      ## alternative.
      set(
        dt, i=remove.i, j=dup.col.indices[[1]],
        value=dt[[remove.col]][remove.i])
    }
    remove.col.list[[dup.name]] <- remove.col.vec
  }
  ## Remove all alternatives other than the first.
  if(length(remove.col.list))set(dt, j=unlist(remove.col.list), value=NULL)
  dt
### data.table with columns defined by calling the functions in
### fun.list on the corresponding column of match.mat. Even if
### fun.list has duplicated names, the output data.table will have
### unique column names (identically named capture groups in
### alternatives will be combined into a single output column).
}

