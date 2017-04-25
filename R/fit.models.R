#' Fit dot Models
#' 
#' Fit a statistical model using different estimators (e.g., robust and
#' least-squares) or combine fitted models into a single object.  Generic
#' methods then produce side-by-side comparisons of the parameter estimates and
#' diagnostic plots.
#' 
#' There are two distinct ways the \code{fit.models} function can be used.
#' 
#' The first is to fit the same model using different estimators.  In this
#' case, \code{model.list} should be a character vector or a list where each
#' element is the name of a modeling function and the remaining arguments (in
#' \dots) are the common arguments to the functions in \code{model.list}.
#' For example, the following command fits robust and least squares linear
#' models to Brownlee's Stack Loss Plant Data. \preformatted{
#' fit.models(c("rlm", "lm"), stack.loss ~ ., data = stackloss)} The resulting
#' \code{fit.models} object is a list with the output of \preformatted{
#' rlm(stack.loss ~ ., data = stackloss)} in the first element and
#' \preformatted{ lm(stack.loss ~ ., data = stackloss)} in the second.  The
#' class attribute of the returned list is set (in this case) to \code{"lmfm"}
#' which is the \code{fit.models} class (fmclass) for comparing linear-model-like
#' fits.
#' 
#' The second use of fit.models is to combine fitted model objects.  In
#' this case, \code{fit.models} combines its arguments into a fit.models object
#' (a list where element \eqn{i} is occupied by argument \eqn{i} and sets the
#' class attribute to the appropriate \code{fit.models} class.
#' 
#' @param model.list a list or a character vector containing names of modeling
#' functions.  Only required when \code{fit.models} is being used to fit models
#' (rather than combine already fitted models into a \code{fit.models} object).
#' @param \dots see details.
#' @return The returned object is a list containing the fitted models.  The
#' class of the retuned object depends on the classes of the model objects it
#' contains.
#' @seealso \code{\link{fmclass.add.class}} for adding a class to an existing
#' fit.models class and \code{\link{fmclass.register}} to create a new
#' fit.models class.
#' @keywords models
#' @examples
#'   # First, use fit.models to fit robust and least squares linear
#'   # regression models to Brownlee's Stack Loss Plant Data.
#' 
#'   # Step 1: rlm (robust linear model) is in the MASS package.
#'   library(MASS)
#' 
#'   # Step 2: tell fit.models rlm can be compared to lm
#'   fmclass.add.class("lmfm", "rlm")
#' 
#'   fm1 <- fit.models(c("rlm", "lm"), stack.loss ~ ., data = stackloss)
#' 
#'   summary(fm1) #rlm does not provide p-values or Multiple R-squared
#' 
#' 
#'   # Second, use fit.models to combine fitted models into a
#'   # fit.models object.
#' 
#'   lm.complete <- lm(stack.loss ~ ., data = stackloss)
#'   lm.clean <- lm(stack.loss ~ ., data = stackloss, subset = 5:20)
#' 
#'   fm2 <- fit.models(lm.clean, lm.complete)
#' 
#'   summary(fm2)
#'   plot(fm2)
#' 
#' 
#'   # Name the models in the fit.models object.
#'   
#'   fm3 <- fit.models(c(Robust = "rlm", "Least Squares" = "lm"),
#'                     stack.loss ~ ., data = stackloss)
#' 
#'   fm4 <- fit.models(Clean = lm.clean, Complete = lm.complete)


#' @export
fit.models <- function(model.list, ...)
{
  fm.call <- match.call()
  fm.call$attributes <- NULL

  dots <- list(...)
  dots.names <- names(dots)

  if(is.null(dots.names))
    dots.names <- character(length(dots))

  supported.classes <- unlist(sapply(e$fmreg, function(u) u$classes))


  ## The only way model list can be missing is if all the arguments in the call
  ## are named. In this case, proceed as if dots is a collection of comparable fitted
  ## models.

  if(missing(model.list)) {
    model.list <- dots
    model.names <- dots.names
  }


  ## Otherwise model.list is either an unnamed (in the call) fitted model or
  ## a list of function names.

  else if(class(model.list)[1] %in% supported.classes) {
    model.list <- c(list(model.list), dots)
    model.names <- c("", dots.names)

    object.names <- as.character(fm.call[-1])
    empty.names <- (nchar(model.names) == 0)
    model.names[empty.names] <- object.names[empty.names]
  }

  else if(is.character(model.list) || class(model.list)[1] == "list") {
    model.list <- as.list(model.list)

    ## Little hack for robust package backward compatibility
    model.list[model.list == "cov"] <- "covMLE"

    n.models <- length(model.list)
    model.funs <- unlist(model.list)

    if(is.null(model.names <- names(model.list)))
      model.names <- model.funs

    model.call <- fm.call
    model.call$model.list <- NULL

    for(i in 1:n.models) {
      model.call[[1]] <- as.name(model.list[[i]])
      model.list[[i]] <- eval(model.call, sys.parent())
    }

    empty.names <- (nchar(model.names) == 0)
    model.names[empty.names] <- model.funs[empty.names]
  }

  else
    stop("impossible error: this should never happen!")

  if(any(nchar(model.names) == 0))
    stop("All models should be named")


  ## Each element of model.list should have a unique name.

  if(length(unames <- unique(model.names)) < length(model.list)) {
    for(n in unames) {
      idx <- (model.names == n)
      if(sum(idx) > 1)
        model.names[idx] <- paste(n, 1:sum(idx), sep = ".")
    }
  }

  names(model.list) <- model.names


  ## Now we should have a properly named list of fitted models.  Have to
  ## set the appropriate attributes.

  candidates <- lapply(e$fmreg, getElement, name = "classes")
  classes <- sapply(model.list, function(u) class(u)[1])

  ## First, the fm class must beable to compare all the classes.

  idx <- sapply(candidates, function(u) all(classes %in% u))
  candidates <- candidates[idx]

  if(!length(candidates)) {
    warning("fit.models cannot compare the provided models")
    return(invisible(model.list))
  }


  ## Try to choose the best comparable class.

  idx <- sapply(candidates, function(u) length(intersect(u, classes)))
  fmclass <- names(which(idx == max(idx)))[1]

  oldClass(model.list) <- c(fmclass, "fit.models")
  model.list
}

