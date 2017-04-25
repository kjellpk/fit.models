#' @importFrom stats coef

#' @export
coef.glm.fit.models <- function(object, ...)
  coef.lm.fit.models(object, ...)


