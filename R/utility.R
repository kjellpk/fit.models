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



par_from_dots <- function(par_name, dots, n.models, mod.names) {
  if (is.null(u <- dots[[par_name]])) {
    u <- stats::setNames(seq_len(n.models), mod.names)
  } else {
    if (length(u) == 1L && n.models > 1L) {
      u <- stats::setNames(rep(u, n.models), mod.names)
    } else if (length(u) == n.models) {
      if (is.null(names(u))) {
        names(u) <- mod.names
      } else {
        u <- u[mod.names]
      }
    } else {
      stop("syntax error: 'col'")
    }
  }

  u
}