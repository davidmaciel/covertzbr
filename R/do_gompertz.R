#' Gompertz curve equation
#'
#' Estimates the values of a gompertz grow process
#'
#' This function uses the same parametrization of the gompertz curve
#' used in the \code{grofit} package.
#'
#' @param time. Integer. A vector of one or more values, counted as the number of days.
#' @param lambda Numeric. A single value for the lambda parameter in the gompertz equation.
#' @param mu Numeric. A single value for the mu parameter in the gompertz equation.
#' @param a Numeric. A single value for de alpha parameter in the gompertz equation.
#'
#' @return a vector of the predict number of total deaths.
#'
#' @export
#'
#' @examples
#' do_gompertz(1:100, lambda = 47, mu = 1000, a = 100000)
do_gompertz <-
function(time, lambda, mu, a){
  a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))

}
