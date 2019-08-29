#' Silvermantest
#'
#' The silvermantest tests the null hypothesis that an underlying density has at most \code{k} modes.
#'
#' @param x vector of data
#' @param k number of modes for the null hypothesis
#' @param R number of bootstrap replications
#' @param adjust boolean to activate the adjusting of the p-value (valid if k=1) (see Hall and York)
#' @param digits number of digits of the p-value
#' @param density.fun A function that returns a vector of density estimates
#'
#' @return An object of the class Silvermantest (see: \code{\link{Silvermantest-class}}).
#'
#' @importFrom stats sd
#' @importFrom stats rnorm
#' @importFrom stats density
#' @importFrom stats predict
#' @importFrom methods new
#' @export
silverman.test <- function(x, k, R=999, adjust=FALSE, digits=6, density.fun=NULL){
  # x: data
  # k: number of modes to be tested
  # M: number of bootstrap replications

  #check if seed is available (as done in boot package)
  #if so save it
  seedAvailable = exists(x=".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(seedAvailable)
    saved_seed = .Random.seed
  else{
    rnorm(1)
    saved_seed = .Random.seed
  }

  #temp function for bootstrapping
  y.obs <- function(x,h,sig=sd(x)){
    mean(x) + (x-mean(x)+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
    #(x+h*rnorm(length(x),0,1))/((1+h^2/sig^2)^(1/2))
  }

  #temp function for density calculation
  if(is.null(density.fun)) {
    density.fun <- function(x,h){density(x,bw=h,kernel ="gaussian")$y}
  }

  #start of the test
  h0 <- h.crit(x, k, density.fun=density.fun)

  # statistic function
  mode.fun <- function(d, i, h0) {
    x.boot <- sort(y.obs(d[i], h0))
    nr.modes(density.fun(x.boot, h0))
  }
  mod.boot <- boot::boot(x, statistic = mode.fun, R = R, h0 = h0)

  n <- sum(as.vector(mod.boot$t) > k)
  p <- n/R

  if (adjust) {
    if (k==1) {
      #asymptotic levels of silvermantest by Hall/York
      x=c(0,0.005,0.010,0.020,0.030,0.040,0.050,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,0.25,0.30,0.35,0.40,0.50)
      y=c(0,0,0,0.002,0.004,0.006,0.010,0.012,0.016,0.021,0.025,0.032,0.038,0.043,0.050,0.057,0.062,0.07,0.079,0.088,0.094,0.102,0.149,0.202,0.252,0.308,0.423)
      sp = splines::interpSpline(x,y)
      #adjusting the p-value
      if (p<0.005)
        p=0
      else{
        p = predict(sp,p)$y
        p = round(p,digits)
      }
    } else{
      print("The option to adjust the p-value is valid only for k=1")
    }
  }

  #return(list(saved_seed=saved_seed,p_value=p))
  test_obj = new("Silvermantest", data=x, p_value = p, saved_seed=saved_seed, k=k)
  return(test_obj)
}
