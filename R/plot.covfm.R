#' Plot Method
#'
#' @description Generic plot method for \dQuote{covfm} objects.
#'
#' @param x a \code{covfm} object.
#' 
#' @param which.plots either "ask", "all", or an integer vector specifying which plots
#'                    to draw.  If which.plots is an integer vector, use the plot numbers
#'                    given here (or in the "ask" menu).  The plot options are
#'                    (2) Sqrt of Mahalanobis Distances, (3) Eigenvalues of Covariance Estimate, 
#'                    (4) Ellipses Matrix, and (5) Distance - Distance Plot.
#'
#' @param ... additional arguments are passed to the plot subfunctions.
#'
#' @return \code{x} is invisibly returned.
#' 
#' @export
plot.covfm <- function(x, which.plots = "all", ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  choices <- c("All",
               "Mahalanobis Distances",
               "Eigenvalues of Covariance Estimate", 
               "Ellipses Plot")

  if(n.models == 2)
    choices <- c(choices, "Distance - Distance Plot")
  else
    which.plots <- which.plots[which.plots != 5]

  all.plots <- 2:length(choices)

  tmenu <- paste("plot:", choices)

  if(is.numeric(which.plots)) {
    if(length(which.plots) == 1 && which.plots == 1)
      which.plots <- all.plots

    if(!all(which.plots %in% all.plots))
      stop(sQuote("which"), " must be in 2:", length(choices))

    if(length(which.plots) == 0)
      return(invisible(x))

    if(length(which.plots) > 1) {
      par.ask <- par(ask = TRUE)
      on.exit(par(ask = par.ask))
    }

    ask <- FALSE
    which.plots <- c(which.plots + 1, 1)
  }

  else if(which.plots == "all") {
    which.plots <- c(all.plots + 1, 1)
    ask <- FALSE
    par.ask <- par(ask = TRUE)
    on.exit(par(ask = par.ask))
  }

  else
    ask <- TRUE

  repeat {
    if(ask) {
      which.plots <- menu(tmenu,
                          title = "\nMake plot selections (or 0 to exit):\n")

      if(any(which.plots == 1)) {
        which.plots <- c(all.plots, 0)
        par.ask <- par(ask = TRUE)
        on.exit(par(ask = par.ask))
      }

    which.plots <- which.plots + 1
    }

    if(!length(which.plots))
      stop(paste("Invalid choice of plot in \'which.plots\'"))

    for(pick in which.plots) {
      switch(pick,
        return(invisible(x)),
        place.holder <- 1,

        distancePlot.covfm(x,
                          xlab = "Index",
                          ylab = "Mahalanobis Distance",
                          pch = 16,
                          ...),

        ellipsesPlot.covfm(x, ...),
        
        screePlot.covfm(x,
                       xlab = "Principal Component",
                       ylab = "Variances",
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

