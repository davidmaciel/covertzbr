
make_df <- function(obs, preds){
  dplyr::left_join(preds, obs, "day_count") %>%
  tidyr::pivot_longer(cols = c(preds, total_deaths),
                                         names_to = "type",
                                         values_to = "total_deaths") %>%
    dplyr::mutate(type = forcats::fct_recode(type, Observadas = "total_deaths",
                                    Previstas = "preds")) %>%
    dplyr::select("Mortes confirmadas" = total_deaths,
                  "Dias" = day_count,
                  "Tipo" = type,
                  lower,
                  max) %>%
    dplyr::mutate("Data" = lubridate::ymd("2020-03-17")+(Dias-1))

}

