#' Register Comparable Functions
#' 
#' The fit.models package maintains a list of comparable models called the
#' fit.models registry.  These functions provide a method for adding new
#' classes of models to the fit.models registry.
#' 
#' See the package vignette.
#' 
#' @aliases fmclass.register fmclass.add.class
#' @param fmclass a character string naming a fit.models class to be added to
#' the \code{fit.models.registry}.
#' @param classes a character vector naming one or more classes that can be
#' compared by the methods defined for the fit.models class in \code{fmclass}.
#' @param class a character string specifying a class compatible with the
#' methods of \code{fmclass}.
#' @param validation.function a function returning \code{TRUE} when the models
#' are comparable.
#' @param warn a logical value. If TRUE, a warning is printed if \code{class}
#' is already registered in the \code{fit.models.registry}.
#' @return a null value is invisibly returned.
#' @keywords misc
#' export
fmclass.register <- function(fmclass, classes, validation.function = NULL)
{
  if(fmclass %in% names(e$fmreg)) {
    warning(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  e$fmreg[[fmclass]] <- list(fmclass = fmclass,
                             classes = classes,
                             validation.function = validation.function)

  invisible()
}


