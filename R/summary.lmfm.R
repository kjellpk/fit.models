#' Comparison Summaries for Linear Regression Models
#' 
#' Compute a summary of each model in an \code{lmfm} object.
#' 
#' 
#' @param object an \code{lmfm} object.
#' @param correlation a logical value.  If TRUE, the correlation matrices for
#' the coefficients are included in the summaries.
#' @param \dots additional arguments required by the generic
#' \code{\link{summary}} function.
#' @return a list with class summary.lmfm whose elements are summaries of each
#' model in \code{object}.
#' @keywords methods regression
#' @examples
#' 
#' data(stackloss)
#' m1 <- lm(stack.loss ~ ., data = stackloss)
#' m2 <- lm(stack.loss ~ . - Acid.Conc., data = stackloss)
#' 
#' fm <- fit.models(m1, m2)
#' print(fm.sum <- summary(fm))


#' @export summary.lmfm
#' @S3method summary lmfm
summary.lmfm <- function(object, correlation = FALSE, ...)
{
  object <- lapply(object, summary, correlation = correlation, ...)
  oldClass(object) <- "summary.lmfm"
  object
}


