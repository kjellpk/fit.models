#' Plot Method
#'
#' @description Generic plot method for \dQuote{covfm} objects.
#'
#' @param x a \code{covfm} object.
#' 
#' @param which.plots either \code{"ask"} (character string) or an integer vector specifying
#'                    which plots to draw.  The plot options are (1) Mahalanobis Distance,
#'                    (2) Ellipses Matrix, (3) Screeplot (Eigenvalues of Covariance Estimate),
#'                    and (4) Distance - Distance Plot.
#'
#' @param ... additional arguments are passed to the plot subfunctions.
#'
#' @return \code{x} is returned invisibly.
#' 
#' @export
plot.covfm <- function(x, which.plots = 1:4, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(length(which.plots) == 1 && casefold(which.plots) == "all")
    which.plots <- 1:4

  choices <- c("Mahalanobis Distance",
               "Ellipses Plot",
               "Screeplot")

  if(n.models == 2) {
    choices <- c(choices, "Distance - Distance Plot")
    all.plots <- 1:4
  }
  else
    all.plots <- 1:3

  if(length(which.plots) == 0)
    return(invisible(x))

  if(is.numeric(which.plots)) {
    which.plots <- intersect(which.plots, all.plots)

    if(length(which.plots) > 1) {
      par.ask <- par(ask = TRUE)
      on.exit(par(ask = par.ask))
    }
    which.plots <- c(which.plots, 0)

    ask <- FALSE
  }

  else
    ask <- TRUE

  repeat {
    if(ask)
      which.plots <- menu(choices, title = "\nMake plot selections (or 0 to exit):")

    if(!length(which.plots))
      stop(paste("Invalid choice of plot in \'which.plots\'"))

    for(pick in (1 + which.plots)) {
      switch(pick,
        return(invisible(x)),

        distancePlot.covfm(x,
                          xlab = "Index",
                          ylab = "Mahalanobis Distance",
                          pch = 16,
                          ...),

        ellipsesPlot.covfm(x, ...),
        
        screePlot.covfm(x,
                       xlab = "Principal Component",
                       ylab = "Variance",
                       main = "Screeplot",
                       ...),

        {
          strip <- paste(mod.names[1], "Distance vs.", mod.names[2], "Distance")
          ddPlot.covfm(x,
                      strip = strip,
                      xlab = paste(mod.names[2], "Distance"),
                      ylab = paste(mod.names[1], "Distance"),
                      main = "Distance-Distance Plot",
                      pch = 16,
                      ...)
        }
      )
    }
  }
  invisible(x)
}

