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
  if(is.null(rownames(match.mat)) && "name" %in% colnames(match.mat)){
    name.vec <- match.mat[, "name"]
    match.df <- data.frame(match.mat, stringsAsFactors=FALSE)
    rownames(match.df) <- 1:nrow(match.df)
    if(any(gone <- is.na(name.vec))){
      print(match.df[gone, ])
      stop("the 'name' group should not be missing/NA")
    }
    name.tab <- table(name.vec)
    not.uniq <- name.tab[1 < name.tab]
    if(length(not.uniq)){
      print(match.df[name.vec %in% names(not.uniq), ])
      stop("capture group named 'name' must be unique")
    }
    rownames(match.mat) <- name.vec
    match.mat <- match.mat[, colnames(match.mat) != "name", drop=FALSE]
  }
  df <- data.frame(match.mat, stringsAsFactors=FALSE)
  for(col.name in names(type.list)){
    if(col.name %in% names(df)){
      type.fun <- type.list[[col.name]]
      tryCatch({
        fun.result <- type.fun(df[[col.name]])
      }, error=function(e){
        stop(
          "type.list must be ",
          "list(group.name=function(character.vector)atomic.vector)")
      })
      if(!is.atomic(fun.result)){
        stop(col.name, " type.list function must return atomic vector")
      }
      if(length(fun.result) != nrow(df)){
        stop(
          col.name,
          " type.list function returned vector of length ",
          length(fun.result),
          " but expected length ",
          nrow(df))
      }
      df[[col.name]] <- fun.result
    }
  }
  df
### If type.list is a list of functions, then return a data.frame
### whose columns are defined by calling the functions in type.list on
### the corresponding column of match.mat. Otherwise just return a
### character matrix. If match.mat does not already have rownames, and
### it has a column named "name", then that column will be used for
### the rownames, and that column will not be returned.
}

