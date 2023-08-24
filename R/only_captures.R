only_captures <- function
### Extract capture group columns from match.mat and assign optional
### groups to "".
(match.mat,
### character matrix.
  stop.fun
### function to call on logical matrix of missing indicators.
){
  group.mat <- match.mat[, -1, drop=FALSE]
  missing.match <- is.na(match.mat[,1])
  stop.fun(missing.match)
  group.mat[is.na(group.mat) & !missing.match] <- "" #optional groups
  group.mat
}
