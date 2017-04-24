#' Overlaid Normal QQ Plot
#' 
#' Produces an overlaid normal QQ plot.
#' 
#' 
#' @param x a \code{fit.models} object.
#' @param fun a function to extract the desired quantity from \code{x}.
#' @param \dots additional arguments are passed to
#' \code{\link[lattice]{qqmath}}.
#' @return the \code{trellis} object is invisibly returned.
#' @keywords hplot


#' @importFrom lattice qqmath strip.default


#' @export
overlaidQQPlot <- function(x, fun, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  y <- lapply(x, fun)
  n.y <- sapply(y, length)
  mod <- factor(rep(mod.names, n.y), levels = mod.names)
  tdf <- data.frame(y = unlist(y), mod = mod)

  p <- qqmath(~ y | "",
              groups = mod,
              data = tdf,
              distribution = qnorm,
              strip = function(...) strip.default(..., style = 1),
              auto.key = list(corner = c(0.05, 0.95)),
              ...)

  print(p)
  invisible(p)
}


