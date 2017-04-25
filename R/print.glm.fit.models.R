#' @export
print.glm.fit.models <- function(x, digits = max(3, getOption("digits") - 3), ...)
  print.lm.fit.models(x, digits = digits, ...)


