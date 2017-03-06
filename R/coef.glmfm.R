#' @importFrom stats coef

#' @S3method coef glmfm
coef.glmfm <- function(object, ...)
  coef.lmfm(object, ...)


