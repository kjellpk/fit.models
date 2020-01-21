
#' @export
vcov.default <- function(object, ...)
{
  if(!is.null(object$cov) && is.matrix(object$cov))
    return(object$cov)
  
  NULL
}


#' @export
location <- function(object, ...)
  UseMethod("location")


#' @export
location.default <- function(object, ...)
{
  if(!is.null(object$center) && is.vector(object$center))
    return(object$center)
  
  NULL
}


#' @export
valid.covfm <- function(x)
  !is.null(vcov(x)) && !is.null(location(x) && all(dim(vcov(x)) == length(location(x))))

