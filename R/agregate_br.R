#' Agregate data in Brazil
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
#' @examples
#' read_covid() %>% agregate_br()
agregate_br <- function(cov){
  cov %>%
    dplyr::filter(is_repeated == F & place_type == "state") %>%
    dplyr::select(date, new_deaths) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(new_deaths = sum(new_deaths)) %>%
    dplyr::mutate(total_deaths = cumsum(new_deaths)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_deaths > 0) %>%
    dplyr::mutate(day_count = 1:nrow(.))
}
