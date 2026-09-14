#' Side-by-Side Mahalanobis Distance Plot
#'
#' @description
#'   Produces side-by-side plots of Mahalanobis distance computed using the
#'   location and covariance matrix estimates contained in each element of a
#'   \code{covfm} object.
#'
#' @param x
#'   a \code{"covfm"} object.
#'
#' @param level
#'   a single numeric value between 0 and 1 giving the chi-squared percent
#'   point used to compute the outlyingness threshold.
#'
#' @param id.n
#'   a single nonnegative integer specifying the number of extreme points to
#'   label in the plot.
#'
#' @param \dots
#'   additional arguments are passed to \code{xyplot}.
#'
#' @return
#'   the \code{trellis} object is invisibly returned.
#'
#' @export
distancePlot.covfm <- function(x, level = 0.95, id.n = 3, ...) {
  n.models <- length(x)
  mod.names <- names(x)

  dists <- lapply(x, function(u) as.matrix(u$dist))
  n <- lengths(dists)
  p <- vapply(x, function(u) length(u$center), -1L)

  #' @importFrom stats setNames
  thresh <- setNames(qchisq(level, df = p), mod.names)

  panel.special <- function(x, y, subscripts, id.n, ...) {
    lvl <- as.character(tdf[[subscripts[[1L]],"mod"]])
    out <- which(y > (vt <- thresh[[lvl]]))
    id.n <- min(id.n, length(out))

    panel.xyplot(x = x, y = y, ...)

    if (id.n > 0) {
      out <- order(y, decreasing = TRUE)[seq_len(id.n)]
      panel.text(x[out], y[out], paste(" ", out, sep = ""), adj = 0)
    }

    panel.abline(h = vt, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, n), levels = mod.names)

  indices <- unlist(lapply(dists, rownames), use.names = FALSE)
  if (!anyNA(dates <- as.Date(indices, optional = TRUE))) {
    indices <- dates
  } else {
    indices <- sequence(n)
  }

  tdf <- data.frame(dists = sqrt(unlist(dists)),
                    index = indices,
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
