# Shared chart styling keeps every view readable on the same light canvas.
chart_theme <- function(chart) {
  chart <- highcharter::hc_chart(chart, backgroundColor = "transparent",
    style = list(fontFamily = "Segoe UI, sans-serif"), spacing = c(16, 16, 12, 8),
    animation = FALSE)
  chart <- highcharter::hc_colors(chart, c("#087f73", "#e49a42", "#5577a3", "#9d78ac", "#589b93", "#ce715f"))
  chart <- highcharter::hc_title(chart, text = NULL)
  chart <- highcharter::hc_credits(chart, enabled = FALSE)
  chart <- highcharter::hc_xAxis(chart, lineColor = "#e6ece9", tickLength = 0,
    labels = list(style = list(color = "#60716d", fontSize = "12px")))
  chart <- highcharter::hc_yAxis(chart, gridLineColor = "#edf1ee", gridLineDashStyle = "ShortDash",
    title = list(style = list(color = "#60716d", fontWeight = "400")),
    labels = list(style = list(color = "#60716d", fontSize = "11px")))
  chart <- highcharter::hc_legend(chart, itemStyle = list(color = "#425650", fontWeight = "400"),
                                symbolRadius = 4)
  chart <- highcharter::hc_plotOptions(chart,
    series = list(animation = FALSE, borderWidth = 0, states = list(inactive = list(opacity = 0.6))))
  chart <- highcharter::hc_tooltip(chart, backgroundColor = "#ffffff", borderColor = "#dbe5de",
    borderRadius = 10, shadow = FALSE, padding = 12, style = list(color = "#203e35", fontSize = "13px"))
  chart$x$conf_opts$lang <- list(decimalPoint = ",", thousandsSep = " ",
    months = c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre"),
    shortMonths = c("janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc."),
    weekdays = c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi"),
    viewFullscreen = "Plein écran", exitFullscreen = "Quitter le plein écran", printChart = "Imprimer",
    downloadPNG = "Télécharger en PNG", downloadJPEG = "Télécharger en JPEG", downloadPDF = "Télécharger en PDF",
    downloadSVG = "Télécharger en SVG", downloadCSV = "Télécharger les données CSV", downloadXLS = "Télécharger les données XLS",
    viewData = "Afficher les données", hideData = "Masquer les données", contextButtonTitle = "Options du graphique",
    accessibility = list(defaultChartTitle = "Graphique", chartContainerLabel = "{title}. Graphique interactif.",
      svgContainerLabel = "Graphique interactif", thousandsSep = " ", credits = "Source du graphique : {creditsStr}",
      screenReaderSection = list(beforeRegionLabel = "Description du graphique : {chartTitle}.",
        afterRegionLabel = "Fin du graphique : {chartTitle}.", endOfChartMarker = "Fin du graphique interactif."),
      legend = list(legendLabelNoTitle = "Afficher ou masquer les séries", legendLabel = "Légende : {legendTitle}", legendItem = "Afficher {itemName}"),
      zoom = list(mapZoomIn = "Agrandir la carte", mapZoomOut = "Réduire la carte", resetZoomButton = "Réinitialiser le zoom"),
      table = list(viewAsDataTableButtonText = "Consulter les valeurs dans un tableau, {chartTitle}", tableSummary = "Valeurs du graphique."),
      exporting = list(chartMenuLabel = "Options du graphique", menuButtonLabel = "Ouvrir les options, {chartTitle}")))
  chart$x$hc_opts$accessibility <- list(point = list(valueDecimals = 1))
  chart <- highcharter::hc_add_dependency(chart, "modules/accessibility.js")
  resources <- system.file("app", "www", package = "shinySignalConso")
  if (!nzchar(resources)) resources <- file.path("inst", "app", "www")
  chart$dependencies <- c(chart$dependencies, list(htmltools::htmlDependency(
    "signalconso-highcharts-accessibility", "0.1.0", src = c(file = normalizePath(resources)),
    script = "highcharts-accessibility.js")))
  chart
}

chart_dates <- function(data) {
  column <- if ("date" %in% names(data)) "date" else "creationdate"
  if (!column %in% names(data)) return(as.Date(character()))
  dates <- data[[column]]
  if (!inherits(dates, "Date")) dates <- as.Date(substr(as.character(dates), 1, 10), format = "%Y-%m-%d")
  dates[!is.na(dates)]
}

graph_timeline <- function(data, granularity = "month", date_range = NULL) {
  granularity <- match.arg(granularity, c("month", "week", "day"))
  dates <- chart_dates(data)
  if (!length(dates)) return(NULL)
  bounds <- if (is.null(date_range)) range(dates) else as.Date(date_range)
  stopifnot(length(bounds) == 2L, !anyNA(bounds), bounds[1] <= bounds[2])
  dates <- dates[dates >= bounds[1] & dates <= bounds[2]]
  bucket_date <- function(x) switch(granularity,
    month = as.Date(format(x, "%Y-%m-01")),
    week = x - (as.integer(format(x, "%u")) - 1L),
    day = x)
  buckets <- bucket_date(dates)
  full_dates <- seq(bucket_date(bounds[1]), bucket_date(bounds[2]), by = granularity)
  counts <- tabulate(match(buckets, full_dates), nbins = length(full_dates))
  points <- function(values) lapply(seq_along(full_dates), function(i) {
    list(as.numeric(full_dates[i]) * 86400000, if (is.finite(values[i])) unname(values[i]) else NULL)
  })
  chart <- chart_theme(highcharter::highchart())
  chart <- highcharter::hc_xAxis(chart, type = "datetime", title = list(text = NULL))
  chart <- highcharter::hc_yAxis(chart, min = 0, title = list(text = "Signalements"), allowDecimals = FALSE)
  chart <- highcharter::hc_add_series(chart, data = points(counts), type = "areaspline", name = "Signalements",
    color = "#087f73", fillOpacity = 0.10, lineWidth = 2.5, marker = list(enabled = FALSE))
  if (granularity == "day") {
    for (window in c(30L, 90L)) {
      if (length(counts) >= window) {
        average <- stats::filter(counts, rep(1 / window, window), sides = 1)
        chart <- highcharter::hc_add_series(chart, data = points(average), type = "line",
          name = paste("Moyenne", window, "jours"), lineWidth = 2, marker = list(enabled = FALSE),
          tooltip = list(valueDecimals = 1))
      }
    }
  }
  chart <- highcharter::hc_legend(chart, enabled = granularity == "day" && length(counts) >= 30L)
  highcharter::hc_tooltip(chart, shared = TRUE, valueDecimals = 0,
    xDateFormat = switch(granularity, month = "%B %Y", week = "Semaine du %e %B %Y", day = "%e %B %Y"))
}

graph_seasonality <- function(data, period = "weekday") {
  period <- match.arg(period, c("weekday", "month"))
  dates <- chart_dates(data)
  if (!length(dates)) return(NULL)
  labels <- if (period == "weekday") c("Lun.", "Mar.", "Mer.", "Jeu.", "Ven.", "Sam.", "Dim.") else
    c("Janv.", "Févr.", "Mars", "Avr.", "Mai", "Juin", "Juil.", "Août", "Sept.", "Oct.", "Nov.", "Déc.")
  index <- as.integer(format(dates, if (period == "weekday") "%u" else "%m"))
  counts <- tabulate(index, nbins = length(labels))
  chart <- chart_theme(highcharter::highchart())
  chart <- highcharter::hc_xAxis(chart, categories = as.list(labels), title = list(text = NULL))
  chart <- highcharter::hc_yAxis(chart, min = 0, title = list(text = "Signalements cumulés"), allowDecimals = FALSE)
  chart <- highcharter::hc_add_series(chart, data = as.list(counts), type = "column", name = "Signalements",
    color = "#087f73", borderRadius = 4, maxPointWidth = 40)
  chart <- highcharter::hc_legend(chart, enabled = FALSE)
  highcharter::hc_tooltip(chart, valueDecimals = 0)
}

graph_territory <- function(data, level = "region") {
  level <- match.arg(level, c("region", "department"))
  if (is.null(data) || !nrow(data)) return(NULL)
  file <- if (level == "region") "fr-regions.geo.json" else "fr-departments.geo.json"
  path <- system.file("app", "www", "maps", file, package = "shinySignalConso")
  if (!nzchar(path)) path <- file.path("inst", "app", "www", "maps", file)
  if (!file.exists(path)) stop("Le fond de carte local est introuvable. Réinstallez les fichiers de l’application.")
  map <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  region_codes <- c("fr-gua" = "01", "fr-mq" = "02", "fr-gf" = "03", "fr-lre" = "04", "fr-may" = "06",
    "fr-idf" = "11", "fr-cvl" = "24", "fr-bfc" = "27", "fr-nor" = "28", "fr-hdf" = "32", "fr-ges" = "44",
    "fr-pdl" = "52", "fr-bre" = "53", "fr-naq" = "75", "fr-occ" = "76", "fr-ara" = "84", "fr-pac" = "93", "fr-cor" = "94")
  map$features <- Filter(function(feature) !is.null(feature$properties[["hc-key"]]), map$features)
  for (i in seq_along(map$features)) {
    properties <- map$features[[i]]$properties
    code <- if (level == "region") unname(region_codes[properties[["hc-key"]]]) else
      sub("^FR-", "", properties[["iso3166-2"]])
    if (identical(code, "75C")) code <- "75"
    map$features[[i]]$properties$code <- code
  }
  if (level == "department") {
    # The source separates Lyon from Rhône; report data use department 69 for both.
    lyon <- which(vapply(map$features, function(x) identical(x$properties$code, "69M"), logical(1)))
    rhone <- which(vapply(map$features, function(x) identical(x$properties$code, "69"), logical(1)))
    if (length(lyon) && length(rhone)) {
      polygons <- function(x) if (x$type == "Polygon") list(x$coordinates) else x$coordinates
      map$features[[rhone]]$geometry <- list(type = "MultiPolygon", coordinates = c(
        polygons(map$features[[rhone]]$geometry), polygons(map$features[[lyon]]$geometry)))
      map$features <- map$features[-lyon]
    }
    french_names <- c("06" = "Alpes-Maritimes", "73" = "Savoie", "74" = "Haute-Savoie",
                      "65" = "Hautes-Pyrénées", "2A" = "Corse-du-Sud", "973" = "Guyane")
    for (i in seq_along(map$features)) {
      code <- map$features[[i]]$properties$code
      if (code %in% names(french_names)) map$features[[i]]$properties$name <- unname(french_names[code])
    }
  }
  column <- if (level == "region") "reg_code" else "dep_code"
  if (!column %in% names(data)) stop("Les données ne contiennent pas les codes géographiques nécessaires.")
  codes <- toupper(trimws(as.character(data[[column]])))
  codes[!is.na(codes) & grepl("^[0-9]$", codes)] <- paste0("0", codes[!is.na(codes) & grepl("^[0-9]$", codes)])
  counts <- if ("N" %in% names(data)) as.numeric(data$N) else rep(1, nrow(data))
  totals <- tapply(counts, codes, sum, na.rm = TRUE)
  known_codes <- vapply(map$features, function(feature) feature$properties$code, character(1))
  mapped <- names(totals) %in% known_codes
  if (!any(mapped)) stop("Aucun code géographique ne correspond au fond de carte.")
  unlocated <- sum(counts, na.rm = TRUE) - sum(totals[mapped], na.rm = TRUE)
  points <- lapply(which(mapped), function(i) list(code = names(totals)[i], value = unname(totals[i])))
  chart <- chart_theme(highcharter::highchart(type = "map"))
  chart <- highcharter::hc_add_series(chart, type = "map", mapData = map, data = points,
    joinBy = c("code", "code"), name = "Signalements", borderColor = "#ffffff", borderWidth = 0.7,
    nullColor = "#eef2ee", allAreas = TRUE,
    states = list(hover = list(color = "#e5ad55", borderColor = "#ffffff")),
    dataLabels = list(enabled = FALSE))
  chart <- highcharter::hc_colorAxis(chart, min = 0, minColor = "#e5f2eb", maxColor = "#087f73",
                                    labels = list(format = "{value:,.0f}"))
  chart <- highcharter::hc_mapNavigation(chart, enabled = TRUE, enableMouseWheelZoom = FALSE,
                                       buttonOptions = list(verticalAlign = "bottom"))
  chart <- highcharter::hc_tooltip(chart, headerFormat = "",
    pointFormat = "<b>{point.name}</b><br>{point.value:,.0f} signalements")
  if (unlocated > 0) chart <- highcharter::hc_subtitle(chart,
    text = paste(format(unlocated, big.mark = " ", scientific = FALSE), "signalements hors carte (territoire non renseigné ou non couvert)"),
    style = list(fontSize = "11px", color = "#60716d"))
  highcharter::hc_credits(chart, enabled = TRUE, text = "Fond : Natural Earth / Highcharts",
                        href = "https://www.highcharts.com/docs/maps/map-collection")
}
