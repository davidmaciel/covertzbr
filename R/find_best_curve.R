#' Find the best gompertz curve
#'
#' Given a grid of parameters combinations, the function estimates the gompertz
#' curve for each of them. Next, it calculates the sum of squared residuals and
#' gets the one with the lowest sum of squared residuals.
#'
#' @param grid. A tibble returned by \code(get_initial_params)
#' @param obs. A vector of observed total deaths.
#' @param parallel. Logical. If true, uses parallel processing from the future and
#' furrr packages. Defautls to true.
#'
#' @return. A named numeric vector with the values of lambda, mu and alpha from the
#' best curve
#'
#' @export
#'
#' @examples
#' \dontrun{
#' obs <- read_covid() %>% agregate_br()
#' obs <- obs$total_deaths
#'
#' get_initial_params(lambda_min = 40, lambda_max = 80,mu_min = 800,
#' mu_max = 2000, a_min = 60000, a_min = 1000000) %>%
#' find_best_curve(obs)
#' }
#'
#'
find_best_curve <-
function(grid, obs,  parallel = T) {
  if (parallel == T) {
    future::plan("cluster")
    grid %>% mutate(res = furrr::future_pmap_dbl(
      list(lambda, mu, a),
      get_residual,
      time = 1:length(obs),
      obs = obs
    )) %>%
      dplyr::filter(res == min(res)) %>%
      dplyr::select(-res) %>% unlist()

  } else {
    grid %>% mutate(res = purrr::pmap_dbl(
      list(lambda, mu, a),
      get_residual,
      time = 1:length(obs),
      obs = obs
    )) %>%
      dplyr::filter(res == min(res))
  }
}
