#' Comparison Diagnostic Plots for Generalized Linear Models
#' 
#' Produces a set of comparison diagnostic plots.  The plot options are
#' \enumerate{
#'   \item (not used)
#'   \item Deviance Residuals vs. Predicted Values,
#'   \item Response vs. Fitted Values,
#'   \item Normal QQ Plot of Pearson Residuals,
#'   \item Normal QQ Plot of Deviance Residuals,
#'   \item Pearson Residuals vs. Mahalanobis Distance,
#'   \item Scale-Location.
#' }
#' 
#' @param x a \code{glmfm} object.
#' @param which.plots either \code{"ask"}, \code{"all"}, or a vector of integer
#' values specifying which plots to draw.  In the latter case, use the plot
#' numbers given in the description above (or in the "ask" menu).  Any other
#' values will be silently ignored.
#' @param \dots other parameters to be passed through to plotting functions.
#' @return \code{x} is invisibly returned.
#' @section Side Effects: The selected plots are drawn on a graphics device.
#' @seealso \code{\link{qqPlot.lmfm}} for 4 and 5 and
#' \code{\link{scatterPlot.lmfm}} for the others.
#' @keywords hplot methods
#' @examples
#' # From ?glm:
#' # A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
#' 
#' clotting <- data.frame(
#'     u = c(5,10,15,20,30,40,60,80,100),
#'     lot1 = c(118,58,42,35,27,25,21,19,18),
#'     lot2 = c(69,35,26,21,18,16,13,12,12))
#' 
#' lot1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
#' lot2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)
#' 
#' fm <- fit.models(lot1, lot2)
#' plot(fm)
#' 


#' @importFrom stats predict residuals fitted model.response model.frame
#' @importFrom graphics par
#' @importFrom utils menu

#' @export plot.glmfm
#' @S3method plot glmfm
plot.glmfm <- function(x, which.plots = c(2, 5, 7, 6), ...)
{
  n.models <- length(x)

  choices <- c("All",
               "Deviance Residuals vs. Predicted Values",
               "Response vs. Fitted Values",
               "Normal QQ Plot of Pearson Residuals",
               "Normal QQ Plot of Deviance Residuals",
               "Pearson Residuals vs. Mahalanobis Distance",
               "Scale-Location")

  all.plots <- 2:length(choices)

  tmenu <- paste("plot:", choices)

  if(is.numeric(which.plots)) {
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

    for(pick in which.plots) {
      switch(pick,
        return(invisible(x)),

        place.holder <- 1,

        scatterPlot.lmfm(x,
                         x.fun = predict,
                         y.fun = function(u) residuals(u, type = "deviance"),
                         xlab = expression(plain("Predicted Values")),
                         ylab = expression(plain("Deviance Residuals")),
                         main = expression(plain("Deviance Residuals vs. Predicted Values")),
                         pch = 16),

        scatterPlot.lmfm(x,
                         x.fun = fitted,
                         y.fun = function(u) model.response(model.frame(u)),
                         xlab = expression(plain("Fitted Values")),
                         ylab = expression(plain("Response")),
                         main = expression(plain("Response vs. Fitted Values")),
                         pch = 16),

        qqPlot.lmfm(x,
                    fun = function(u) residuals(u, type = "pearson"),
                    xlab = expression(plain("Standard Normal Quantiles")),
                    ylab = expression(plain("Empirical Quantiles of Pearson Residuals")),
                    main = expression(plain("Normal QQ Plot of Pearson Residuals")),
                    envelope = FALSE,
                    pch = 16),

        qqPlot.lmfm(x,
                    fun = function(u) residuals(u, type = "deviance"),
                    xlab = expression(plain("Standard Normal Quantiles")),
                    ylab = expression(plain("Empirical Quantiles of Deviance Residuals")),
                    main = expression(plain("Normal QQ Plot of Deviance Residuals")),
                    envelope = FALSE,
                    pch = 16),

        scatterPlot.lmfm(x,
                         x.fun = function(u) sqrt(designMD(u)),
                         y.fun = function(v) residuals(v, type = "pearson"),
                         xlab = expression(plain("Mahalanobis Distance")),
                         ylab = expression(plain("Pearson Residuals")),
                         main = expression(plain("Pearson Residuals vs. Mahalanobis Distance")),
                         pch = 16),

        scatterPlot.lmfm(x,
                         x.fun = predict,
                         y.fun = function(u) sqrt(abs(residuals(u, type = "deviance"))),
                         xlab = expression(plain("Predicted Values")),
                         ylab = expression(sqrt(abs(plain("Deviance Residuals")))),
                         main = expression(plain("Scale-Location")),
                         pch = 16)
      )
    }
  }

  invisible(x)
}


