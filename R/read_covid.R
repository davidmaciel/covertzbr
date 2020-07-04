#' Get Covid-19 data from Brasil.io
#'
#' Get all the available data from Brasil.io api, at city level.
#'
#'
#' @return
#' @export
#'
read_covid <- function(){
readr::read_csv("https://data.brasil.io/dataset/covid19/caso_full.csv.gz",locale = readr::locale(encoding = "UTF-8"))
  }



