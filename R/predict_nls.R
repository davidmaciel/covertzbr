#' Non-linear model and prediction intervals for the best curve
#'
#' Given a a non linear model by generates predictions and predictions  intervals
#'
#' @param m Model, returned by \code{model_nls()}
#' @param min_time Integer. Starting value of the time span, counted as number of days.
#' @param max_time Integer. Ending value of the time span, counted as number of days.
#' @param alfa Numeric. Alpha level for the prediction interval
#' @param obs Numeric. A vector of observed deaths, to be appended in the result for comparison
#' @param data Date. A vector of dates, to be apprended in the result for comparison.
#'
#' @return A tibble.
#' @export
#'
predict_nls <-
function(m, min_time, max_time, alfa = 0.05, obs, data){
    propagate::predictNLS(m,
               newdata = data.frame(t = min_time:max_time),
               interval = "prediction", alpha = alfa) %>%
    purrr::pluck("summary") %>%
    dplyr::select("Prop.Mean.2", contains("%")) %>%
    dplyr::select(contains("Prop")) %>%
    dplyr::rename("preds" = 1,"lower" = 2,"max" = 3) %>%
    dplyr::mutate_all(round) %>%
    dplyr::mutate(
      lower = if_else(lower < 0, 0, lower),
      obs = obs,
      data = data
    )
  }
