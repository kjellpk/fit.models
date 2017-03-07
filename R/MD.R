#' Mahalanobis Distance
#' 
#' Returns the Mahalanobis distance of all rows in the design matrix.
#' 
#' For least-squares linear models the Mahalanobis distance is computed from
#' the associated diagonal value of the hat matrix. This function is generic.
#' The default method assigns a distance of \eqn{\sqrt{(n - 1)(p - 1)}{n}}
#' to each row.
#' 
#' @aliases MD MD.default MD.lm
#' @param object a fitted model object.
#' @param \dots additional arguments are passed to the dispatched method.
#' @return a numeric vector containing the Mahala
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
#' MD(lm.D9)


#' @importFrom stats lm.influence


#' @rdname MD
#' @export MD
MD <- function(object, ...)
  UseMethod("MD")


#' @rdname MD
#' @S3method MD default
MD.default <- function(object, ...)
{
  p <- length(coef(object))
  n <- length(fitted(object))
  rep(sqrt((n - 1) * (p - 1) / n), n)
}


#' @rdname MD
#' @S3method MD lm
MD.lm <- function(object, ...)
{
  p <- length(coef(object))
  n <- length(fitted(object))
  h <- lm.influence(object, do.coef = FALSE)$hat
  sqrt((n - 1) * (h - 1/n))
}

