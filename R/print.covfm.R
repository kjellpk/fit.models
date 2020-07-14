#' @export
print.covfm <- function(x, corr = FALSE, digits = max(3, getOption("digits") - 3), ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  acc <- if(corr) vcor else vcov
  
  calls <- lapply(x, display.call)

  cat("\nCalls: \n")
  for(i in names(calls))
    cat(paste0(i, ": ", calls[[i]], "\n"))

  p <- nrow(acc(x[[1]]))
  i1 <- rep(seq(p), times = p)
  i2 <- rep(seq(p), each = p)

  cov.index <- paste("[", paste(i1, i2, sep = ","), "]", sep = "")
  cov.index <- matrix(cov.index, p, p)
  cov.index <- cov.index[row(cov.index) >= col(cov.index)]

  cov.unique <- t(sapply(x, function(u) (v <- acc(u))[row(v) >= col(v)]))
  dimnames(cov.unique) <- list(mod.names, cov.index)

  cat("\nComparison of Covariance/Correlation Estimates:\n")
  cat(" (unique covariance terms) \n")
  print(cov.unique, digits = digits, ...)

  loc <- t(sapply(x, center))
  loc.names <- names(center(x[[1]]))
  dimnames(loc) <- list(mod.names, loc.names)

  cat("\nComparison of center Estimates: \n")
  print(loc, digits = digits, ...)

  invisible(x)
}

