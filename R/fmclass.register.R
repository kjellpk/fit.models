#' Register Comparable Functions
#' 
#' The \code{fit.models} package maintains a list of comparable models.
#' These functions provide an api to modify this list.
#' 
#' See the package vignette.
#' 
#' @aliases fmclass.register fmclass.add.class
#' @param fmclass a character string naming the fit.models class to be added.
#' @param classes a character vector naming one or more classes that can be
#' compared by the methods defined for the fit.models class in \code{fmclass}.
#' @param class a character string specifying a class compatible with the
#' methods of \code{fmclass}.
#' @param validation.function a function returning \code{TRUE} when the models
#' are comparable.
#' @param warn a logical value. If TRUE, a warning is printed if \code{class}
#' is already registered.
#' @return a null value is invisibly returned.
#' @keywords misc

#' @rdname fmclass
#' @export
fmclass.register <- function(fmclass, classes, validation.function = NULL)
{
  if(fmclass %in% names(e$fmreg)) {
    message(fmclass, " is already registered in the fit.models registry")
    return(invisible())
  }

  e$fmreg[[fmclass]] <- list(fmclass = fmclass,
                             classes = classes,
                             validation.function = validation.function)

  invisible()
}


#' @rdname fmclass
#' @export
fmclass.add.class <- function(fmclass, class, warn = TRUE)
{
  if(class %in% e$fmreg[[fmclass]]$classes && warn) {
    message(class, " is already registered in the fit.models registry")
    return(invisible())
  }
  
  e$fmreg[[fmclass]]$classes <- union(e$fmreg[[fmclass]]$classes, class)
  
  invisible()
}


