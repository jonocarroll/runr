# group source lines into complete expressions (using a brutal-force method)
group_src_V8 = function(code, context) {
  if ((n <- length(code)) < 1) return(list(code))
  i = i1 = i2 = 1
  x = list()
  while (i2 <= n) {
    piece = code[i1:i2]
    if (context$validate(piece) &&  context$eval(piece)!="undefined") {
      x[[i]] = piece; i = i + 1
      i1 = i2 + 1 # start from the next line
    }
    i2 = i2 + 1
  }
  if (i1 <= n) parse(text = piece)  # must be an error there
  x
}

# V8 engine - use in knitr by setting knit_engines$set(V8=V8engine)
V8engine <- function(options) {
  require(V8)
  if(!exists(".context")){
    if(is.null(options$V8.context)){
      assign(".context", new_context(), envir = .GlobalEnv)
    }else{
      .context <<- options$V8.context
    }
    if(!is.null(options$V8.libraries)){
      for(library in options$V8.libraries){
        .context$source(library)
      }
    }
  }
  code <- as.character(c(options$code))
  if(!.context$validate(code)) stop("invalid javascript code")
  if(!is.element(options$results, c("hide", "last"))){
    code <- group_src_V8(code, .context)
    output <- sapply(code, function(code) .context$eval(code))
    code <-  sapply(code, function(code) knitr:::wrap_rmd(text = paste(code, collapse = "\n")))
  }else{
    if(options$results=="last") output <- .context$eval(code)
    code <- knitr:::wrap_rmd(text = paste(code, collapse = "\n"))
    if(options$results=="hide") return(code)
    return(c(code, knitr:::msg_wrap(output, type = "message", options = options)))
  }
  knitr::engine_output(options, code, output)
}
