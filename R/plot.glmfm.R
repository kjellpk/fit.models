#' Comparison Diagnostic Plots for Generalized Linear Models
#' 
#' Produces a set of comparison diagnostic plots.  The plot options are
#' \enumerate{
#'   \item Deviance Residuals vs. Predicted Values,
#'   \item Response vs. Fitted Values,
#'   \item Normal QQ Plot of Pearson Residuals,
#'   \item Normal QQ Plot of Deviance Residuals,
#'   \item Pearson Residuals vs. Mahalanobis Distance,
#'   \item Sqrt Deviance Residuals vs. Predicted Values.
#' }
#' 
#' @param x a \code{glmfm} object.
#' @param which.plots either \code{"ask"} (character string) or an integer
#' vector specifying which plots to draw. In the later case, the plot numbers
#' are given above.
#' @param \dots other parameters to be passed through to plotting functions.
#' @return \code{x} is invisibly returned.
#' @section Side Effects: The selected plots are drawn on a graphics device.
#' @seealso \code{\link{sideBySideQQPlot}} for 4 and 5 and
#' \code{\link{sideBySideScatterPlot}} for the others.
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

#' @export
plot.glmfm <- function(x, which.plots = 1:6, ...)
{
  if(length(which.plots) == 1 && casefold(which.plots) == "all")
    which.plots <- 1:6

  choices <- c("Deviance Residuals vs. Predicted Values",
               "Response vs. Fitted Values",
               "Normal QQ Plot of Pearson Residuals",
               "Normal QQ Plot of Deviance Residuals",
               "Pearson Residuals vs. Mahalanobis Distance",
               "Sqrt Deviance Residuals vs. Predicted Values")

  all.plots <- 1:6

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

        sideBySideScatterPlot(x,
                              x.fun = predict,
                              y.fun = function(u) residuals(u, type = "deviance"),
                              xlab = expression(plain("Predicted Values")),
                              ylab = expression(plain("Deviance Residuals")),
                              main = expression(plain("Deviance Residuals vs. Predicted Values")),
                              pch = 16),

        sideBySideScatterPlot(x,
                              x.fun = fitted,
                              y.fun = function(u) model.response(model.frame(u)),
                              xlab = expression(plain("Fitted Values")),
                              ylab = expression(plain("Response")),
                              main = expression(plain("Response vs. Fitted Values")),
                              pch = 16),

        sideBySideQQPlot(x,
                         fun = function(u) residuals(u, type = "pearson"),
                         xlab = expression(plain("Standard Normal Quantiles")),
                         ylab = expression(plain("Empirical Quantiles of Pearson Residuals")),
                         main = expression(plain("Normal QQ Plot of Pearson Residuals")),
                         envelope = FALSE,
                         pch = 16),

        sideBySideQQPlot(x,
                         fun = function(u) residuals(u, type = "deviance"),
                         xlab = expression(plain("Standard Normal Quantiles")),
                         ylab = expression(plain("Empirical Quantiles of Deviance Residuals")),
                         main = expression(plain("Normal QQ Plot of Deviance Residuals")),
                         envelope = FALSE,
                         pch = 16),

        sideBySideScatterPlot(x,
                              x.fun = function(u) sqrt(designMD(u)),
                              y.fun = function(v) residuals(v, type = "pearson"),
                              xlab = expression(plain("Mahalanobis Distance")),
                              ylab = expression(plain("Pearson Residuals")),
                              main = expression(plain("Pearson Residuals vs. Mahalanobis Distance")),
                              pch = 16),

        sideBySideScatterPlot(x,
                              x.fun = predict,
                              y.fun = function(u) sqrt(abs(residuals(u, type = "deviance"))),
                              xlab = expression(plain("Predicted Values")),
                              ylab = expression(sqrt(abs(plain("Deviance Residuals")))),
                              main = expression(paste(sqrt(abs(plain("Deviance Residuals"))), plain(" vs. Predicted Values"))),
                              pch = 16)
      )
    }
  }

  invisible(x)
}


