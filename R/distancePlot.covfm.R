#' Side-by-Side Mahalanobis Distance Plot
#' 
#' @description Produces side-by-side plots of Mahalanobis distance computed using the
#'              location and covariance matrix estimates contained in each element of a
#'              \code{covfm} object.
#'
#' @param x a \code{"covfm"} object.
#' 
#' @param level a single numeric value between 0 and 1 giving the chi-squared percent
#'              point used to compute the outlyingness threshold.
#'              
#' @param id.n a single nonnegative integer specifying the number of extreme points to
#'             label in the plot.
#'             
#' @param \dots additional arguments are passed to \code{xyplot}.
#'
#' @return the \code{trellis} object is invisibly returned.
#'
#' @export
distancePlot.covfm <- function(x, level = 0.95, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  dists <- lapply(x, function(u) u$dist)
  n <- sapply(dists, length)
  p <- sapply(x, function(u) nrow(u$cov))

  thresh <- qchisq(level, df = p)

  for(i in 1:n.models)
    dists[[i]] <- c(thresh[i], dists[[i]])

  panel.special <- function(x, y, id.n, ...) {
    x <- x[-1]
    vt <- y[1]
    y <- y[-1]
    n <- length(y)
    out <- which(y > vt)
    id.n <- min(id.n, length(out))

    panel.xyplot(x, y, ...)

    if(id.n > 0) {
      out <- order(y)[(n-id.n+1):n]
      panel.text(x[out], y[out], paste(" ", out, sep = ""), adj = 0)
    }

    panel.abline(h = vt, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, n+1), levels = mod.names)

  tdf <- data.frame(dists = sqrt(unlist(dists)),
                    index = unlist(lapply(n, function(u) 0:u)),
                    mod = mod)

  p <- xyplot(dists ~ index | mod,
              data = tdf,
              panel = panel.special,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              id.n = id.n,
              ...)

  print(p)
  invisible(p)
}

