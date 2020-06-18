
#' Find the best gompertz curve
#'
#' Given a set of possible parameters for the gompertz curve, finds the best
#' candidate, which is the one with the minimal sum of least squared residuals
#'
#' @param grid A tibble returned from \code{get_initial_params}
#' @param obs A vector of observed deaths
#' @param parallel Logical. If true, uses parallel processing for speed up.
#'
#' @return A named vector of three values: lamba, mu and a
#' @export
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
