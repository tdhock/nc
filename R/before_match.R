before_match <- structure(function
### Augment pattern so that it can be used to match an entire string. Useful for complex find and replace operations, when used with capture_all_str.
(...
### pattern as in capture_first_vec.
){
  nc::alternatives(
    ## (?s) single line (dotall) makes . match anything (even a newline).
    ## Changes of these options within a group are automatically cancelled at the end of the group.
    list(before="(?s).*?", match=list(...)),
    ## from https://www.pcre.org/current/doc/html/pcre2syntax.html#TOC1
    ##   $           end of subject
    ##                 also before newline at end of subject
    ##                 also before internal newline in multiline mode
    ##   \Z          end of subject
    ##                 also before newline at end of subject
    ##   \z          end of subject
    list(before="(?s).+?", "\\z")) #because $ does not work.
### Pattern with two new groups, before (everything before ...) and match (text matching ... or blank for end of string).
}, ex=function(){

  markdown_link <- list(
    "\\[",
    title=".*?",
    "\\]\\(",
    url="http.*?",
    "\\)")
  markdown_subject <- "before [foo](http) between [bar text](http) after\n"
  nc::capture_all_str(markdown_subject, markdown_link)
  
  before_link <- nc::before_match(markdown_link)
  (all_dt <- nc::capture_all_str(markdown_subject, before_link))

  ## before + match = full subject.
  identical(all_dt[, paste(paste0(before, match), collapse="")], markdown_subject)

  ## replace with org link.
  all_dt[, paste(paste0(before, ifelse(
    match=="", "", sprintf("[[%s][%s]]", url, title)
  )), collapse="")]

  ## also works with no extra text before/after match.
  nc::capture_all_str("[foo](http) between [bar text](http)", before_link)

})
