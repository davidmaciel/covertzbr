#' Possible values for the Gompertz curves
#'
#' Given a min and and a max values for the three parameters in the gompertz equation,
#' generates a sequence of possible values and all the combinations of them.
#'
#' @param lambda_min. Numeric. Minimun possible value for the lambda parameter in the gompertz
#' equation.
#' @param lambda_max Numeric. Maximun possible value for the lambda parameter in the gompertz
#' equation.
#' @param mu_min Numeric. Minimun possible value for the mu parameter in the gompertz
#' equation.
#' @param mu_max Numeric. Maximum possible value for the mu parameter in the gompertz
#' equation
#' @param a_min Numeric. Minimun possible value for the alpha parameter in the gompertz
#' equation.
#' @param a_max Numeric. Maximum possible value for the alpha parameter in the gompertz
#' equation.
#' @param int_lambda Numeric. Increment of the sequence for the possible values of the lambda parameter.
#' @param int_mu Numeric. Increment of the sequence for the possible values of the mu parameter.
#' @param int_a Numeric. Increment of the sequence for the possible values of the alpha parameter.
#'
#' @return a tibble, with every possible combination between the values in the sequences
#' of each parameter.
#' @export
get_initial_params <-
function(lambda_min, lambda_max, mu_min, mu_max, a_min, a_max,
                      int_lambda = 1, int_mu = 10, int_a = 100){
  lambda <- seq(lambda_min, lambda_max, by = int_lambda)
  mu <- seq(mu_min, mu_max, by = int_mu)
  a <- seq(a_min, a_max, by = int_a)
  expand.grid("lambda" = lambda,"mu" = mu,"a" = a)
}
