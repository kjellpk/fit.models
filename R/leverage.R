#' Leverage
#' 
#' Returns a leverage measure for each sample point in the data.
#' 
#' For least-squares linear models the leverages are the diagonal elements of
#' the hat matrix.  This function is generic.  The default method assigns a
#' leverage of \eqn{p/n} to every sample point.
#' 
#' @aliases leverage leverage.default leverage.lm
#' @param object a fitted model object.
#' @param \dots additional arguments are passed to the dispatched method.
#' @return a numeric vector containing the leverages.
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
#' leverage(lm.D9)
#' 
leverage <- function(object, ...)
  UseMethod("leverage")


leverage.default <- function(object, ...)
{
  p <- length(coef(object))
  n <- length(fitted(object))
  rep(p/n, n)
}


leverage.lm <- function(object, ...)
  lm.influence(object, do.coef = FALSE)$hat



