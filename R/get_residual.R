#' Residuals for the Gompertz Curve
#'
#' Calculate the sum of squared residuals for a gompertz curve, given
#' a vector of observed total deaths.
#'
#' @param time Integer. A vector of one or more values, counted as the number of days.
#' @param obs Numeric. A vector of obseverd total deaths. It must be of the same length as \code{time}
#' @param lambda Numeric. A single value for the lambda parameter in the gompertz equation.
#' @param mu Numeric. A single value for the mu parameter in the gompertz equation.
#' @param a Numeric. A single value for de alpha parameter in the gompertz equation.
#'
#' @return Numeric. A single value, representing the sum of squared residuals.
#'
#'
get_residual <-
function(time, obs, lambda, mu, a){
  curva <- do_gompertz(time, lambda = lambda, mu = mu, a = a)
  sum((obs-curva)^2)
}
