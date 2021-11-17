### Try to run a capture function. If it fails we wrap the error
### message with a more informative message that also includes the
### generated pattern.
try_or_stop_print_pattern <- function(expr, pat, engine){
  tryCatch(expr, error=function(e){
    stop(domain=NA, gettextf("%s
when matching pattern above with %s engine, an error occured: %s", pat, engine, e$message))
  })
}
