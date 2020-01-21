#' Summary Method
#'
#' @description Generic summary method for \dQuote{covfm} objects.
#'
#' @param object a \dQuote{covfm} object.
#' 
#' @param ... additional arguments for the summary method.
#'
#' @export
summary.covfm <- function(object, ...)
{
  object <- lapply(object, summary, ...)
  oldClass(object) <- "summary.covfm"
  object
}


