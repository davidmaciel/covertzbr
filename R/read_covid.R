#' Read Covid-19 Data
#'
#' Imports the most recent covid-19 data of Brazil from \url{https://brasil.io/covid19/}
#'
#' @return A tibble. Each row corresponds to a city/state and a date since de
#' beggining of the COVID-19 epidemic in Brazil.
#' @export
#'
read_covid <- function(){
  readr::read_csv("https://brasil.io/dataset/covid19/caso_full/?format=csv")
}
