#' Mahalanobis Distance
#' 
#' Returns the squared Mahalanobis distance of all rows in \code{x} and the vector
#' \eqn{\mu} = \code{center} with respect to \eqn{\Sigma} = \code{cov}. This is
#' (for vector \code{x}) defined as \deqn{D^2 = (x - \mu)' \Sigma^{-1} (x - \mu).}
#' The \code{fit.models} package makes the \code{mahalanobis} function generic.  The
#' default method calls the \code{\link[stats]{mahalanobis}} function in the
#' \code{stats} package.  Methods for fitted model objects return the squared
#' Mahalanobis distance of all the rows in the design (model) matrix.
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
#' @keywords regression methods
#' @examples
#' stack.lm <- lm(stack.loss ~ ., data = stackloss)
#' 
#' # Mahalanobis distance (not squared)
#' sqrt(mahalanobis(stack.lm))


#' @importFrom stats mahalanobis hatvalues model.matrix terms var

#' @export
mahalanobis <- function(x, ...)
  UseMethod("mahalanobis")


#' @export
#' @describeIn mahalanobis the default S3 method calls \code{stats::}\code{\link[stats]{mahalanobis}}
mahalanobis.default <- function(x, center, cov, inverted = FALSE, ...)
  stats::mahalanobis(x, center, cov, inverted, ...)


#' @export
#' @describeIn mahalanobis the method for \code{lm} objects returns the squared mahalanobis distance
#' for each row in the design (model) matrix. These values are computed from the diagonal elements
#' of the hat matrix.
mahalanobis.lm <- function(x, ...)
{
  n <- length(h <- hatvalues(x))
  (n - 1) * (h - 1/n)
}



# Should catch most S3 lm-like classes:

#' @export
mahalanobis.list <- function(x, ...)
{
  tr <- terms(x)
  mm <- model.matrix(x)
  
  if(attr(tr, "intercept") == 1)
    mm <- mm[, -1, drop = FALSE]
  
  stats::mahalanobis(mm, colMeans(mm), var(mm))
}


#' @export
mahalanobis.rlm <- function(x, ...)
  mahalanobis.list(x, ...)


