try_or_stop_print_pattern <- function(expr, pat, engine){
  tryCatch(expr, error=function(e){
    print(pat)
    stop(
      "when matching pattern printed above with ",
      engine, " engine, an error occured: ",
      e$message)
  })
}
