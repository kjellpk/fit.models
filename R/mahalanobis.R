#' Mahalanobis Distance
#' 
#' Returns the squared Mahalanobis distance.
#' 
#' The \code{fit.models} package makes the \code{mahalanobis} function generic.
#' The default method calls \code{stats::mahalanobis}. The method provided for
#' linear model objects (class = \code{lm}) returns the squared Mahalanobis
#' distance of each row in the design matrix.  These values are computed from
#' the associated diagonal value of the hat matrix.
#' 
#' @aliases mahalanobis mahalanobis.default mahalanobis.lm
#' @param x a vector or matrix of data with, say, p columns. If \code{x} is
#' a fitted model object then the design matrix (model matrix) is used.
#' @param center mean vector of the distribution or second data vector of
#' length p or recyclable to that length.  If set to \code{FALSE}, the
#' centering step is skipped. This argument is ignored when \code{x} is a
#' fitted model object.
#' @param cov covariance matrix (p x p) of the distribution. This argument
#' is ignored when \code{x} is a fitted model object.
#' @param inverted logical.  If \code{TRUE}, \code{cov} is supposed to contain
#' the \emph{inverse} of the covariance matrix.  This argument is ignored when
#' \code{x} is a fitted model object.
#' @param \dots passed to \code{\link[base]{solve}} for computing the inverse
#' of the covariance matrix (if \code{inverted} is \code{FALSE}).  Additional
#' arguments are ignored when \code{x} is a fitted model object.
#' @return a numeric vector containing the squared Mahalanobis distances
#' @seealso The \code{\link[stats]{mahalanobis}} function in the \code{stats}
#' package for the default method.
#' @keywords regression methods
#' @examples
#' 
#' ## Example from lm:
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2,10,20, labels=c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' 
#' #
#' sqrt(mahalanobis(lm.D9))


#' @importFrom stats mahalanobis hatvalues model.matrix terms var

#' @rdname mahalanobis
#' @export mahalanobis
mahalanobis <- function(x, ...)
  UseMethod("mahalanobis")


#' @rdname mahalanobis
#' @method mahalanobis default
#' @S3method mahalanobis default
mahalanobis.default <- function(x, center, cov, inverted = FALSE, ...)
  stats::mahalanobis(x, center, cov, inverted, ...)


#' @rdname mahalanobis
#' @method mahalanobis lm
#' @S3method mahalanobis lm
mahalanobis.lm <- function(x, ...)
{
  n <- length(h <- hatvalues(x))
  (n - 1) * (h - 1/n)
}


#' @S3method mahalanobis list
mahalanobis.list <- function(x, ...)
{
  tr <- terms(x)
  mm <- model.matrix(x)
  
  if(attr(tr, "intercept") == 1)
    mm <- mm[, -1, drop = FALSE]
  
  stats::mahalanobis(mm, colMeans(mm), var(mm))
}


#' @S3method mahalanobis rlm
mahalanobis.rlm <- function(x, ...)
  mahalanobis.list(x, ...)


