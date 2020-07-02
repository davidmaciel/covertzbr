#' Aggregate data in Brazil
#'
#' Takes a tibble returned by \code{read_covid} and summarises it onto
#' country level
#'
#' @param cov A tibbe returned by \code{read_covid}
#'
#' @return A tibble, in which each row corresponds to a day since the first confirmed
#' death from COVID-19 in Brazil.
#' @export
#'
aggregate_br <- function(cov){
  x<-cov %>%
    dplyr::select(date, deaths) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(total_deaths = sum(deaths)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_deaths > 0) %>%
    dplyr::mutate(day_count = 1:nrow(.))
  if(x$total_deaths[nrow(x)] < x$total_deaths[nrow(x)-1]){
    x<-slice(x, 1:nrow(x)-1)
  }
  x
}
