
#' @importFrom stats vcov

#' @export
vcov.default <- function(object, ...)
{
  if(!is.null(object$cov) && is.matrix(object$cov))
    return(object$cov)
  
  NULL
}


#' Extract Location Estimate for a Fitted Model Object
#'
#' @description Returns the location estimated from a
#'              location/scatter-type fitted model object.
#'
#' @param object a fitted model object.
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

