#' The Silvermantest-class.
#'
#' @slot data The underlying test data
#' @slot p_value The p-value of the test
#' @slot saved_seed The random seed that was used during calculation
#' @slot k The number of modes tested
#'
#' @rdname Silvermantest-class
#' @name Silvermantest-class
methods::setClass("Silvermantest", representation = representation(data = "numeric", p_value = "numeric", saved_seed = "numeric", k = "numeric"))

methods::setMethod("show", signature(object="Silvermantest"),
  function(object) {
    cat("Silvermantest: Testing the null hypothesis that the number of modes is <= ", object@k, "\n")
    cat("The resulting p-value is ", object@p_value, "\n")
  }
)



