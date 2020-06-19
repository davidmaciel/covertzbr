#' Non linear modelling of the gompertz curve
#'
#' Given a vector of observations, estimates a non linear model by
#' weighted least squares using the gompertz equation and the values of parameters from
#' the best curve found trough \code{find_best_curve} as initial values
#'
#'
#'
#' @param obs Numeric. A vector of observed total deaths.
#' @param best_curve A vector, returned from \code{find_best_curve}
#'
#' @return. A non-linear model object
#'
#' @export
#'
model_nls <-
function(best_curve, obs){
  nls(formula("y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"),
      data = data.frame(y = obs, t = 1:length(obs)),
      start = best_curve)

}
