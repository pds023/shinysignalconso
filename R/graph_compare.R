#' Compare distributions across selected groups
#' @param data Report-level data.
#' @param groupColumn Column containing the categories to display.
#' @param input_var Column defining the groups being compared.
#' @param input_mod Selected group values.
#' @param input_pct Counts (niv or nb) or percentages within each group.
#' @return A Highcharter widget.
#' @export
graph_compare <- function(data, groupColumn, input_var, input_mod, input_pct = "percent") {
  if (is.null(data) || !nrow(data) || !length(input_var) || !length(input_mod)) return(NULL)
  if (!all(c(groupColumn, input_var) %in% names(data))) return(NULL)
  labels <- as.character(data[[groupColumn]])
  labels[is.na(labels) | !nzchar(labels)] <- "Non renseigné"
  groups <- as.character(data[[input_var]])
  groups[is.na(groups) | !nzchar(groups)] <- "Non renseigné"
  selected <- groups %in% input_mod
  if (!any(selected)) return(NULL)
  counts <- table(labels[selected], factor(groups[selected], levels = unique(input_mod)))
  counts <- counts[order(-rowSums(counts), rownames(counts)), , drop = FALSE]
  totals <- colSums(counts)
  limited <- nrow(counts) > 12L
  counts <- head(counts, 12L)
  percentage <- !input_pct %in% c("niv", "nb")
  chart <- chart_theme(highcharter::highchart())
  chart <- highcharter::hc_xAxis(chart, categories = as.list(as.character(htmltools::htmlEscape(rownames(counts)))),
    title = list(text = if (limited) "12 principales catégories · triées par volume cumulé" else NULL))
  chart <- highcharter::hc_yAxis(chart, min = 0, max = if (percentage) 100 else NULL,
    title = list(text = if (percentage) "Part dans chaque groupe (%)" else "Signalements"),
    labels = list(format = if (percentage) "{value} %" else "{value:,.0f}"))
  for (i in seq_len(ncol(counts))) {
    shares <- if (totals[i] > 0) 100 * counts[, i] / totals[i] else rep(0, nrow(counts))
    points <- lapply(seq_len(nrow(counts)), function(j) {
      list(y = if (percentage) unname(shares[j]) else unname(counts[j, i]),
           count = unname(counts[j, i]), share = unname(shares[j]))
    })
    chart <- highcharter::hc_add_series(chart, data = points, name = as.character(htmltools::htmlEscape(colnames(counts)[i])),
                                      type = "bar", borderRadius = 3)
  }
  highcharter::hc_tooltip(chart, shared = TRUE,
    pointFormat = "<span style='color:{series.color}'>●</span> {series.name} : <b>{point.count:,.0f}</b> ({point.share:.1f} %)<br>")
}
