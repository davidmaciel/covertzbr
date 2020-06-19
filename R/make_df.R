
make_df <- function(obs, preds){
  dplyr::left_join(preds, obs, "date") %>%
  tidyr::pivot_longer(cols = c(preds, total_deaths),
                                         names_to = "type",
                                         values_to = "total_deaths") %>%
    dplyr::mutate(type = forcats::fct_recode(type, Observadas = "total_deaths",
                                    Previstas = "preds")) %>%
    dplyr::select("Mortes confirmadas" = total_deaths,
                  "Data" = date,
                  "Tipo" = type,
                  lower,
                  max)
}

