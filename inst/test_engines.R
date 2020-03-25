available.engines <- character()
for(e in c("RE2", "ICU", "PCRE")){
  is.available <- tryCatch({
    stop_for_engine(e)
    TRUE
  }, error=function(msg){
    FALSE
  })
  if(is.available){
    available.engines[[e]] <- e
  }
}
test_engines <- function(desc, ...){
  for(e in available.engines){
    old.opt <- options(nc.engine=e)
    test_that(paste(e, desc), ...)
    options(old.opt)
  }
}
