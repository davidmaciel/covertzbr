#' Plot a highchart
#'
#' given a tibble of predicted and observed total deaths, with a column indicating
#' dates, plots a highchart of observed x predicted values, along with prediction
#' intervals
#'
#' @param preds. A tibble, returned by \code{predic_nls}
#'
#' @return
#' @export
#'
plot_highchart <-
function(preds){
  col <- RColorBrewer::brewer.pal(3, "Set1")
  col <- col[1:2]
  col <- GISTools::add.alpha(col, 0.8)
  preds <- preds %>% tidyr::pivot_longer(cols = c(preds,obs),
                                         names_to = "type",
                                         values_to = "total_deaths") %>%
    dplyr::mutate(type = fct_recode(type, Observados = "obs",
                                    Previstos = "preds")) %>%
    dplyr::select("?bitos confirmados" = total_deaths,
                  "Data" = data,
                  "Tipo" = type,
                  lower,
                  max)
  prev <- preds %>% filter(Tipo != "Observados")
  highcharter::hchart(preds, "line",
         hcaes(x = Data, y = `?bitos confirmados`, group = Tipo),
         opacity = 0) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_xAxis(name = "Data") %>%
    highcharter::hc_colors(col) %>%
    highcharter::hc_add_series(prev, name = "Intervalo", type = "arearange",
                  hcaes(x = Data, y = `?bitos confirmados`,
                        low = lower,
                        high = max),
                  color = hex_to_rgba("gray", 0.1),
                  zIndex = -2,
                  showInLegend = F) %>%
    highcharter::hc_tooltip(shared = T) %>%
    highcharter::hc_title(text = "<b>?bitos confirmados por COVID-19 no Brasil</b>") %>%
    highcharter::hc_subtitle(text = "<b>Observados x previstos</b>")
}
