#' Bandwidth calculation for density estimation
#'
#' Calculates the smallest value so that the gaussian kernel density estimate of the given data \code{x} has \code{k} modes.
#' The smaller you choose the bandwidth for a kernel density estimate, the larger the number of modes becomes. This function calculates the smallest value leading to a kernel density estimate with \code{k} number of modes.
#'
#' @param x vector of data
#' @param k number of modes
#' @param prec number of digits for precision of calculation
#' @param density.fun A function that returns a vector of density estimates
#'
#' @return the smallest value so that the gaussian kernel density estimate of the given data \code{x} has \code{k} modes.
#'
#' @examples h.crit(rnorm(10), k = 1)
#'
#' @export
h.crit <- function(x, k, prec = 6, density.fun = NULL) {
  if (is.null(density.fun)) {
    density.fun <- function(x, h) {
      density(x, bw = h, kernel = "gaussian")$y
    }
  }

  digits <- prec
  prec <- 10 ^ (-prec)
  x <- sort(x)
  minh <- min(diff(x))		#minimal possible h
  maxh <- diff(range(x)) / 2	#maximal possible h
  a <- maxh
  b <- minh
  zaehler <- 0

  while (abs(b - a) > prec) {
    m <- nr.modes(density.fun(x, a))

    b <- a
    if (m > k) {
      minh <- a
      a <- (a + maxh) / 2
    }
    else {
      maxh <- a
      a <- (a - minh) / 2
    }
  }
  a <- round(a, digits)

  if (nr.modes(density.fun(x, a)) <= k) {
    #subtract until more than k modes
    while (nr.modes(density.fun(x, a)) <= k) {
      a <- a - prec
    }
    a <- a + prec
  }
  if (nr.modes(density.fun(x, a)) > k) {
    #add until nr. of moodes correct
    while (nr.modes(density.fun(x, a)) > k) {
      a <- a + prec
    }
  }
  a
}
