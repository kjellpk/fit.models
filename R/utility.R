display.call <- function(object)
{
  if(is.null(the.call <- object$call)) {
    x <- class(object)[1]
    if(substring(x, 1, 8) == "summary.")
      x <- substring(x, 9)
    paste0(x, "(...)")
  }
  else
    format(the.call)
}

