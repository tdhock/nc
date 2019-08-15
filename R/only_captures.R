### Extract capture group columns from match.mat and assign optional
### groups to "".
only_captures <- function(match.mat, stop.fun){
  group.mat <- match.mat[, -1, drop=FALSE]
  missing.match <- is.na(match.mat[,1])
  stop.fun(missing.match)
  group.mat[is.na(group.mat) & !missing.match] <- "" #optional groups
  group.mat
}
