#' Plot densities for multiple modes.
#'
#' @param x Vector of data.
#' @param modes The modes to plot density estimates for.
#' @param mark_modes Whether or not to indicate modes in the plot.
#' @param in_one Whether to produce one plot. Default is true for \code{length(modes) < 4}.
#'
#' @examples densities.plot(c(rnorm(10), rnorm(10, mean = 10)))
#'
#' @importFrom stats density
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics plot
#' @export
densities.plot <- function(x, modes = 1:4, mark_modes = TRUE, in_one = TRUE * (length(modes) < 4)) {
    #temp function to identify x-,y- densityvalues of the modes
    mode_ind = function(x, b) {
      y <- density(x, bw = b)$y
      d1 <- diff(y)
      signs <- diff(d1 / abs(d1))
      ind <- which(signs == -2)
      return(ind)
    }

    #the fuction should only be started if all modes are integers
    if (!any(modes != as.integer(modes))) {
      n <- length(modes)
      m <- ceiling(sqrt(n))
      h0_min <- h.crit(x, max(modes))
      h0_max <- h.crit(x, min(modes))
      y_max <- max(density(x, bw = h0_min)$y)
      x_max <- max(abs(density(x, bw = h0_max)$x))
      #plot all densiities in one graphic
      if (in_one == TRUE) {
        h0 <- h.crit(x, modes[1])
        d <- density(x, bw = h0)
        plot(
          d,
          main <- paste("densities.plot"),
          xlim <- c(-x_max, x_max),
          ylim <- c(0, y_max),
          xlab <- ""
        )
        for (i in 2:n) {
          h0 <- h.crit(x, modes[i])
          d <- density(x, bw = h0)
          lines(d, lty = i)
          #mark modes if option selected
          if (mark_modes == TRUE && modes[i] == max(modes)) {
            ind <- mode_ind(x, h0)
            x_values <- d$x
            y_values <- d$y

            for (j in 1:modes[i]) {
              #lines(c(x_values[ind[j]],x_values[ind[j]]), c(0,y_max),col="red")
              lines(c(x_values[ind[j]], x_values[ind[j]]),
                    c(y_values[ind[j]] - 0.025, y_values[ind[j]] + 0.025),
                    col = "red")
            }
          }
        }
      }
      #plot each densiities in one graphic
      else {
        #set plottng device
        oldpar <- par(mfrow = c(m,m))
        on.exit(par(oldpar))

        for (i in 1:n) {
          h0 <- h.crit(x, modes[i])
          d <- density(x, bw = h0)
          plot(d,
               main = paste("modes:", modes[i]),
               ylim = c(0, y_max))
          #mark modes if option selected
          if (mark_modes == TRUE) {
            ind <- mode_ind(x, h0)
            x_values <- d$x
            y_values <- d$y
            for (j in 1:modes[i]) {
              lines(c(x_values[ind[j]], x_values[ind[j]]),
                    c(y_values[ind[j]] - 0.025, y_values[ind[j]] + 0.025),
                    col = "red")
            }
          }
        }
      }
    }
    else{
      stop("Only integer values are allowed for modes.")
    }
  }
