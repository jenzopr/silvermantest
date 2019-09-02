#' Plot p-values along different number of modes.
#'
#' P-values are obtained using the \code{silverman.test} function with the number of modes ranging from \code{kmin} to \code{kmax}.
#'
#' @param x The vector of data to use for testing.
#' @param kmin,kmax The number of modes.
#' @param alpha Significance level, shown as a dashed line in the resulting plot.
#' @param adjust Perform p-value adjustment in case of \code{k=1} (see Hall and York).
#'
#' @return The function does not return anything, but creates a plot.
#'
#' @examples silverman.plot(x = c(rpois(n = 50, lambda = 1), rnorm(n = 100, mean = 4)))
#'
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @export
silverman.plot <- function(x, kmin = 1, kmax = 5, alpha = 0.05, adjust = FALSE) {
    pvalues <- 0
    if (kmax < kmin) {
      warning("kmax was smaller than kmin.")
      t <- kmin
      kmin <- kmax
      kmax <- t
    }
    else{
      for (i in kmin:kmax) {
        if (i == 1 && adjust == TRUE)
          temp <- silverman.test(x, k = i, adjust = TRUE)
        else
          temp <-  silverman.test(x, k = i)
        pvalues[i] <- temp@p_value
      }

      plot(
        pvalues,
        ylim = c(0, 1),
        pch = 20,
        main = "Plot of p-values",
        xlab = "Number of modes in null hypothesis of silvermantest",
        ylab = "p-value"
      )
      lines(pvalues, lty = 3)
      lines(rep(alpha, kmax), col = "red", lty = 3)
    }
  }
