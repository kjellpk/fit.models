
#' @importFrom stats vcov

#' @export
vcov.default <- function(object, ...)
{
  if(!is.null(object$cov) && is.matrix(object$cov))
    return(object$cov)
  
  NULL
}


#' Calculate Correlation Matrix for a Fitted Model Object
#'
#' @description Retrieve a correlation matrix estimate from a
#'              fitted model object. The default method uses
#'              \code{\link[stats]{cov2cor}} to scale the
#'              covariance matrix returned by
#'              \code{\link[stats]{vcov}}.
#'
#' @param object a fitted model object, typically.  Sometimes
#'               also a \code{summary()} object of such a fitted
#'               model.
#'
#' @param \dots additional arguments for method functions.
#' 
#' @export
vcor <- function(object, ...)
  UseMethod("vcor")



#' @export
vcor.default <- function(object, ...)
{
  #' @importFrom stats vcov
  if(!is.null(v <- vcov(object)))
    #' @importFrom stats cov2cor
    return(cov2cor(v))
  
  NULL
}


#' Calculate Location Estimate for a Fitted Model Object
#'
#' @description Returns the location estimated from a
#'              location/scatter-type fitted model object.
#'
#' @param object a fitted model object, typically.  Sometimes
#'               also a \code{summary()} object of such a fitted
#'               model.
#'
#' @param \dots additional arguments for method functions.
#' 
#' @export
center <- function(object, ...)
  UseMethod("center")


#' @export
center.default <- function(object, ...)
{
  if(!is.null(object$center) && is.vector(object$center))
    return(object$center)
  
  NULL
}


valid.covfm <- function(x)
  !is.null(vcov(x)) && !is.null(center(x) && all(dim(vcov(x)) == length(center(x))))

