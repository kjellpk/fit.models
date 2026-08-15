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



par_from_dots <- function(par_name, dots, mod.names) {
  if (!is.null(u <- dots[[par_name]])) {
    if (length(u) == 1L && length(mod.names) > 1L) {
      u <- stats::setNames(rep(u, length(mod.names)), mod.names)
    } else if (length(u) == length(mod.names)) {
      if (is.null(names(u))) {
        names(u) <- mod.names
      } else {
        u <- u[mod.names]
      }
    }
    if (!anyNA(u)) {
      return(u)
    }
  }

  stats::setNames(seq_along(mod.names), mod.names)
}
