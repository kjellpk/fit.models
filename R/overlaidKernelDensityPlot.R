#' Overlaid Kernel Density Estimate Plot
#' 
#' Produces an overlaid kernel density plot.
#' 
#' 
#' @param x a \code{fit.models} object.
#' @param fun a function to extract the desired quantity from \code{x}.
#' @param \dots additional arguments are passed to
#' \code{densityplot}.
#' @return the \code{trellis} object is invisibly returned.
#' @keywords hplot


#' @importFrom lattice densityplot strip.default


#' @export
overlaidKernelDensityPlot <- function(x, fun, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  y <- lapply(x, fun)
  n.y <- sapply(y, length)
  mod <- factor(rep(mod.names, n.y), levels = mod.names)
  tdf <- data.frame(y = unlist(y), mod = mod)

  p <- densityplot(~ y | "",
                   groups = mod,
                   data = tdf,
                   n = 256,
                   bw = "SJ",
                   plot.points = FALSE,
                   strip = function(...) strip.default(..., style = 1),
                   auto.key = list(corner = c(0.95, 0.95)),
                   ...)

  print(p)
  invisible(p)
}


