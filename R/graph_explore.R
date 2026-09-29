#' Plot a distribution of reports
#' @param data A two-column table containing labels and counts.
#' @param input_type Chart type: bar or treemap (pie is a legacy alias).
#' @param input_pct Counts (niv or nb) or percentages.
#' @param group Whether the first of three columns contains a group label.
#' @param max_items Maximum displayed categories, including the remaining categories.
#' @return A Highcharter widget.
#' @export
graph_explore <- function(data, input_type = "bar", input_pct = "niv", group = FALSE, max_items = 12L) {
  if (is.null(data) || !nrow(data)) return(NULL)
  values <- as.data.frame(data)
  names(values) <- if (group) c("group", "label", "N") else c("label", "N")
  values$label <- as.character(values$label)
  values$label[is.na(values$label) | !nzchar(values$label)] <- "Non renseigné"
  if (group) values$label <- paste(values$group, values$label, sep = " · ")
  values <- stats::aggregate(N ~ label, values, sum)
  values <- values[is.finite(values$N) & values$N > 0, ]
  if (!nrow(values)) return(NULL)
  values <- values[order(-values$N, values$label), ]
  total <- sum(values$N)
  # Preserve the denominator and the long tail in one readable category.
  if (nrow(values) > max_items) {
    keep <- seq_len(max_items - 1L)
    values <- rbind(values[keep, ],
                    data.frame(label = "Autres catégories", N = sum(values$N[-keep])))
  }
  percentage <- !input_pct %in% c("niv", "nb")
  labels <- as.character(htmltools::htmlEscape(values$label))
  points <- lapply(seq_len(nrow(values)), function(i) {
    list(name = labels[i], y = if (percentage) 100 * values$N[i] / total else values$N[i],
         value = values$N[i], count = values$N[i], share = 100 * values$N[i] / total)
  })
  chart <- chart_theme(highcharter::highchart())
  chart <- highcharter::hc_tooltip(chart, headerFormat = "",
    pointFormat = "<b>{point.name}</b><br>{point.count:,.0f} signalements<br>{point.share:.1f} % du total")
  if (input_type %in% c("treemap", "pie")) {
    chart <- highcharter::hc_add_series(chart, data = points, type = "treemap",
      name = "Signalements", layoutAlgorithm = "squarified", colorByPoint = TRUE,
      borderColor = "#ffffff", borderWidth = 3,
      dataLabels = list(enabled = TRUE, format = "{point.name}",
                        style = list(textOutline = "none", fontSize = "12px")))
  } else {
    chart <- highcharter::hc_xAxis(chart, categories = as.list(labels), title = list(text = NULL),
      labels = list(style = list(fontSize = "12px", textOverflow = "ellipsis")))
    chart <- highcharter::hc_yAxis(chart, min = 0, max = if (percentage) 100 else NULL,
      title = list(text = if (percentage) "Part des signalements (%)" else "Signalements"),
      labels = list(format = if (percentage) "{value} %" else "{value:,.0f}"))
    chart <- highcharter::hc_add_series(chart, data = points, type = "bar", name = "Signalements",
      color = "#087f73", borderRadius = 4, maxPointWidth = 24,
      dataLabels = list(enabled = TRUE, format = if (percentage) "{point.y:.1f} %" else "{point.y:,.0f}",
                        style = list(fontWeight = "600", color = "#334a4d", textOutline = "none")))
  }
  highcharter::hc_legend(chart, enabled = FALSE)
}
