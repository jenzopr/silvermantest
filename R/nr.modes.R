#' Number of modes
#'
#' Calculates the number of modes for given y-values of a density function.
#'
#' @param y vector of y-values of a density function
#'
#' @export
nr.modes <- function(y) {
  d1 <- diff(y)
	signs <- diff(d1/abs(d1))
	length(signs[signs==-2])
}
