#' Summary Method
#'
#' @description Generic summary method for \dQuote{covfm} objects.
#'
#' @param object a \dQuote{covfm} object.
#' 
#' @param corr a logical value passed as an attribute to the \code{print}
#'             method. When \code{TRUE}, correlations are compared in the
#'             textual output.
#' 
#' @param \dots additional arguments for the summary method.
#'
##' @export
summary.covfm <- function(object, corr = FALSE, ...)
{
  object <- lapply(object, summary, ...)
  attr(object, "corr") <- corr
  oldClass(object) <- "summary.covfm"
  object
}


