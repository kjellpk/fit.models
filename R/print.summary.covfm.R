#' @export
print.summary.covfm <- function(x, corr = FALSE, digits = max(3, getOption("digits") - 3),
                                print.distance = FALSE, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(!("corr" %in% names(match.call())[-1]) && !is.null(ac <- attr(x, "corr")))
    corr <- ac

  acc <- if(corr) vcor else vcov
  
  calls <- lapply(x, display.call)

  cat("\nCalls: \n")
  for(i in names(calls))
    cat(paste0(i, ": ", calls[[i]], "\n"))

  p <- dim(acc(x[[1]]))[1]
  i1 <- rep(seq(p), times = p)
  i2 <- rep(seq(p), each = p)

  cov.index <- paste("[", paste(i1, i2, sep = ","), "]", sep = "")
  cov.index <- matrix(cov.index, p, p)
  cov.index <- cov.index[row(cov.index) >= col(cov.index)]

  cov.unique <- t(sapply(x, function(u) (v <- acc(u))[row(v) >= col(v)]))
  dimnames(cov.unique) <- list(mod.names, cov.index)

  cat("\nComparison of Covariance/Correlation Estimates:\n")
  cat(" (unique correlation terms) \n")
  print(cov.unique, digits = digits, ...)

  loc <- t(sapply(x, center))
  loc.names <- names(center(x[[1]]))
  dimnames(loc) <- list(mod.names, loc.names)

  cat("\nComparison of center Estimates: \n")
  print(loc, digits = digits, ...)

  evals <- t(sapply(x, function(u) u$evals))
  eval.names <- names(x[[1]]$evals)
  dimnames(evals) <- list(mod.names, eval.names)

  cat("\nComparison of Eigenvalues: \n")
  print(evals, digits = digits, ...)

  have.dist <- sapply(x, function(u) !is.null(u$dist))
  if(print.distance && all(have.dist)) {
    dists <- t(sapply(x, function(u) u$dist))
    dimnames(dists) <- list(mod.names, names(x[[1]]$dist))
    cat("\nComparison of Mahalanobis Distances: \n")
    print(dists, digits = digits, ...)
  }

  invisible(x)
}

