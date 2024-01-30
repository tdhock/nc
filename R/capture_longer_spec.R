capture_longer_spec <- structure(function
### Create a spec data table for input to tidyr::pivot_longer_spec.
(data,
### Data table to reshape (actually the data are ignored, and only the
### column names are used).
  ...,
### Regex and conversion as described in capture_first_vec. This is
### processed by measure so if "column" is used as an argument name
### then there will be multiple output columns in the reshaped data
### (analogous to names_to=".value" in tidyr::pivot_longer).
  values_to="value"
### string to use for name of value column in output (only used if
### there is a single output column in the reshaped data).
){
  .name <- NULL
  ## above to avoid CRAN check NOTE.
  cols <- names(data)
  vec.or.list <- nc::measure(..., cols=cols)
  vtab <- attr(vec.or.list, "variable_table")
  if(is.integer(vec.or.list)){
    vec.or.list <- structure(list(vec.or.list), names=values_to)
  }
  spec.list <- list()
  for(.value in names(vec.or.list)){
    index.vec <- vec.or.list[[.value]]
    spec.list[[.value]] <- data.table(.name=cols[index.vec], vtab, .value)
  }
  do.call(rbind, spec.list)[!is.na(.name)]
### data table describing a reshape longer operation.
}, ex=function(){

  (one.iris <- iris[1,])
  (single.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", dim=".*", values_to="cm"))
  (multiple.spec <- nc::capture_longer_spec(iris, part=".*", "[.]", column=".*"))
  if(requireNamespace("tidyr")){
    tidyr::pivot_longer_spec(one.iris, single.spec)
    tidyr::pivot_longer_spec(one.iris, multiple.spec)
  }
  
})
