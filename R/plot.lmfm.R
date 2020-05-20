#' Comparison Diagnostic Plots for Linear Regression Models
#' 
#' Produces a set of comparison diagnostic plots.  The plot options are
#' \enumerate{
#'   \item Normal QQ Plot of Residuals,
#'   \item Kernel Density Estimate of Residuals,
#'   \item Residuals vs. Mahalanobis Distance,
#'   \item Residuals vs. Fitted Values,
#'   \item Sqrt Residuals vs. Fitted Values,
#'   \item Response vs. Fitted Values,
#'   \item Residuals vs. Index (Time),
#'   \item Overlaid Normal QQ Plot of Residuals,
#'   \item Overlaid Kernel Density Estimate of Residuals,
#'   \item Scatter Plot with Overlaid Fits (for simple linear regression models).
#'   }
#' 
#' @param x an \code{lmfm} object.
#' @param which.plots either \code{"ask"} (character string) or an integer
#' vector specifying which plots to draw. In the later case, the plot numbers
#' are given above.
#' @param \dots additional parameters are ignored.
#' @return \code{x} is invisibly returned.
#' @section Side Effects: The selected plots are drawn on a graphics device.
#' @seealso See \code{\link{sideBySideQQPlot}} for 2,
#' \code{\link{sideBySideKernelDensityPlot}} for 3,
#' \code{\link{sideBySideIndexPlot}} for 8,
#' \code{\link{overlaidQQPlot}} for 9,
#' \code{\link{overlaidKernelDensityPlot}} for 10,
#' \code{\link{overlaidSimpleRegressionPlot}} for 11, and
#' \code{\link{sideBySideScatterPlot}} for the others.
#' @keywords hplot methods
#' @examples
#' data(stackloss)
#' stack.lm <- lm(stack.loss ~ ., data = stackloss)
#' stack.clean <- lm(stack.loss ~ ., data = stackloss, subset = 5:20)
#' fm <- fit.models(stack.clean, stack.lm)
#' plot(fm)


# The \emph{modified residuals} are defined to be
# 
# \deqn{r_{i} = \frac{e_{i}}{\sqrt{1 - h_{i}}}}
# 
# where \eqn{h_{i} = H_{ii}} is the \eqn{i^{th}} diagonal element of the hat
# matrix.  The modified residuals are identically distributed with variance
# \eqn{\sigma^{2}}.  The modified residuals are used instead of the
# standardized residuals (which are identically distributed with variance 1)
# so that the comparison plots emphasize differences in the variance
# estimates.


#' @importFrom stats residuals fitted model.response model.frame
#' @importFrom graphics par
#' @importFrom utils menu


#' @export 
plot.lmfm <- function(x, which.plots = 1:10, ...)
{
  if(length(which.plots) == 1 && casefold(which.plots) == "all")
    which.plots <- 1:10

  choices <- c("Normal QQ Plot of Residuals", 
               "Kernel Density Estimate of Residuals",
               "Residuals vs. Mahalanobis Distance",
               "Residuals vs. Fitted Values", 
               "Sqrt Residuals vs. Fitted Values", 
               "Response vs. Fitted Values", 
               "Residuals vs. Index (Time)", 
               "Overlaid Normal QQ Plot of Residuals", 
               "Overlaid Kernel Density Estimate of Residuals")

  is.simple.reg <- function(m)
    all(attr(m$terms, "dataClasses") == "numeric") && dim(model.frame(m))[2] == 2

  if(all(sapply(x, is.simple.reg))) {
    choices <- c(choices, "Scatter Plot with Overlaid Fit(s)")
    all.plots <- 1:10
  }
  else
    all.plots <- 1:9

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

  n.models <- length(x)
  if(n.models <= 3)
    colors <- c("black", "blue", "purple")[1:n.models]
  else
    colors <- 1:n.models

  repeat {
    if(ask)
      which.plots <- menu(choices, title = "\nMake plot selections (or 0 to exit):")

    for(pick in (1 + which.plots)) {
      switch(pick,
        return(invisible(x)),

        sideBySideQQPlot(x,
                         fun = residuals,
                         main = expression(plain("Normal QQ Plot of Residuals")),
                         xlab = expression(plain("Standard Normal Quantiles")),
                         ylab = expression(plain("Empirical Quantiles of Residuals")),
                         pch = 16),

        sideBySideKernelDensityPlot(x,
                                    fun = residuals,
                                    main = expression(plain("Kernel Density Estimate of Residuals")),
                                    xlab = expression(plain("Residuals")),
                                    ylab = expression(plain("Density"))),

        sideBySideScatterPlot(x,
                              x.fun = function(u) sqrt(designMD(u)),
                              y.fun = residuals,
                              xlab = expression(plain("Mahalanobis Distance")),
                              ylab = expression(plain("Residuals")),
                              main = expression(plain("Residuals vs. Mahalanobis Distance")),
                              pch = 16),

        sideBySideScatterPlot(x,
                              x.fun = fitted,
                              y.fun = residuals,
                              main = expression(plain("Residuals vs. Fitted Values")),
                              xlab = expression(plain("Fitted Values")),
                              ylab = expression(plain("Residuals")),
                              pch = 16),

        sideBySideScatterPlot(x,
                              x.fun = fitted,
                              y.fun = function(u) sqrt(abs(residuals(u))),
                              main = expression(paste(sqrt(abs(plain("Residuals"))), plain(" vs. Fitted Values"))),
                              xlab = expression(plain("Fitted Values")),
                              ylab = expression(sqrt(abs(plain("Residuals")))),
                              pch = 16),

        sideBySideScatterPlot(x,
                              x.fun = fitted,
                              y.fun = function(u) model.response(model.frame(u)),
                              main = expression(plain("Response vs. Fitted Values")),
                              xlab = expression(plain("Fitted Values")),
                              ylab = expression(plain("Response")),
                              pch = 16),

        sideBySideIndexPlot(x,
                            fun = residuals,
                            main = expression(plain("Residuals vs. Index (Time)")),
                            xlab = expression(plain("Index (Time)")),
                            ylab = expression(plain("Residuals")),
                            pch = 16),

        overlaidQQPlot(x,
                       fun = residuals,
                       main = expression(plain("Normal QQ Plot of Residuals")),
                       xlab = expression(plain("Standard Normal Quantiles")),
                       ylab = expression(plain("Empirical Quantiles of Residuals")),
                       pch = 16),

        overlaidKernelDensityPlot(x,
                                  fun = residuals,
                                  main = expression(plain("Kernel Density Estimate of Residuals")),
                                  xlab = expression(plain("Residuals")),
                                  ylab = expression(plain("Density"))),

        overlaidSimpleRegressionPlot(x,
                                     main = expression(plain("Scatter Plot with Overlaid Fits")),
                                     lwd.reg = n.models:1,
                                     col.reg = colors,
                                     pch = 16)
      )
    }
  }

  invisible(x)
}





