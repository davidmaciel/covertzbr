#' Plot a highchart
#'
#' given a tibble of predicted and observed total deaths, with a column indicating
#' dates, plots a highchart of observed x predicted values, along with prediction
#' intervals
#' @param obs A tibble, with two columns: "date", of Date type, and a vector of total
#' deaths
#' @param preds A tibble, returned by \code{predic_nls}
#'
#' @return a highchart
#' @export
plot_highchart <-
function(obs, preds){
  col <- RColorBrewer::brewer.pal(3, "Set1")
  col <- col[1:2]
  col <- GISTools::add.alpha(col, 0.8)
preds_obs <- make_df(obs, preds)
  highcharter::hchart(preds_obs, "line",
         highcharter::hcaes(x = Data, y = `Mortes confirmadas`, group = Tipo),
         opacity = 0) %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_xAxis(name = "Data") %>%
    highcharter::hc_yAxis(floor = 0) %>%
    highcharter::hc_colors(colors = col) %>%
    highcharter::hc_add_series(preds, name = "Intervalo", type = "arearange",
                  highcharter::hcaes(x = date, y = preds,
                        low = lower,
                        high = max),
                  color = highcharter::hex_to_rgba("gray", 0.1),
                  zIndex = -2,
                  showInLegend = F) %>%
    highcharter::hc_tooltip(shared = T) %>%
    highcharter::hc_title(text = "<b> Mortes confirmadas por COVID-19 no Brasil</b>") %>%
    highcharter::hc_subtitle(text = "<i> Valores Observados x previstos</i>")
}
