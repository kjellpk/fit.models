#' @importFrom stats coef

#' @export
coef.glmfm <- function(object, ...)
  coef.lmfm(object, ...)


