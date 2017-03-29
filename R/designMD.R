#' Design Matrix Mahalanobis Distance
#' 
#' Returns the squared Mahalanobis distance of all rows in the design (model)
#' matrix \eqn{X} and the sample mean vector \eqn{\mu} of the columns
#' of \eqn{X} with respect to the sample covariance matrix \eqn{\Sigma}.
#' This is (for vector \eqn{x'} a row of \eqn{X}) defined as
#' \deqn{d^{2} = (x - \mu)' \Sigma^{-1} (x - \mu)}
#' where
#' \deqn{\mu = colMeans(X)}
#' and
#' \deqn{\Sigma = cov(X).}
#' 
#' @aliases designMD designMD.default
#' @param object a fitted model object with a \code{\link{model.matrix}} method.
#' @param \dots additional arguments are ignored.
#' @return a numeric vector containing the squared Mahalanobis distances.
#' @keywords regression methods
#' @examples
#' stack.lm <- lm(stack.loss ~ ., data = stackloss)
#' 
#' # Mahalanobis distance (not squared)
#' sqrt(designMD(stack.lm))

#' @importFrom stats mahalanobis model.matrix terms var


#' @export
designMD <- function(object, ...)
  UseMethod("designMD")


#' @export
designMD.default <- function(object, ...)
{
  tr <- terms(object)
  x <- model.matrix(object)
  
  if(attr(tr, "intercept") == 1)
    x <- x[, -1, drop = FALSE]
  
  stats::mahalanobis(x, colMeans(x), var(x))
}


