#' The application server
#' @param input,output,session Internal Shiny parameters.
#' @import shiny data.table
#' @noRd
app_server <- function(input, output, session) {
  source_error <- NULL
  source_info <- tryCatch(load_signalconso_data(), error = function(e) {
    source_error <<- conditionMessage(e)
    list(data = NULL, mode = "error", label = "Source indisponible", detail = "")
  })
  data <- source_info$data
  date_bounds <- if (!is.null(data) && nrow(data)) range(data$date) else as.Date(c(NA, NA))
  filter_columns <- c(category = "category", region = "reg_name", departement = "dep_name",
    subcategories = "subcategories", tags = "tags", sigstate = "signalement_traitement")
  default_filters <- c(list(dates = date_bounds), setNames(rep(list(character()), length(filter_columns)), filter_columns))
  applied_filters <- reactiveVal(default_filters)
  pending_filters <- reactive({
    values <- lapply(names(filter_columns), function(key) {
      value <- input[[paste0("exploration_filter_", key)]]
      if (is.null(value)) character() else sort(as.character(value))
    })
    names(values) <- filter_columns
    dates <- input$filter_dates
    if (is.null(dates) || length(dates) != 2L) dates <- date_bounds
    c(list(dates = as.Date(dates)), values)
  })
  if (!is.null(data) && nrow(data)) {
    updateDateRangeInput(session, "filter_dates", start = date_bounds[1], end = date_bounds[2],
      min = date_bounds[1], max = date_bounds[2])
    for (key in names(filter_columns)) {
      shinyWidgets::updatePickerInput(session, paste0("exploration_filter_", key),
        choices = sort(unique(data[[filter_columns[[key]]]])), selected = character())
    }
  }
  observeEvent(input$exploration_filters_apply, {
    filters <- pending_filters()
    if (anyNA(filters$dates) || filters$dates[1] > filters$dates[2]) {
      showNotification("Choisissez une période valide : la fin doit suivre le début.", type = "warning")
      return()
    }
    applied_filters(filters)
  })
  observeEvent(input$exploration_filters_reset, {
    for (key in names(filter_columns)) {
      shinyWidgets::updatePickerInput(session, paste0("exploration_filter_", key), selected = character())
    }
    updateDateRangeInput(session, "filter_dates", start = date_bounds[1], end = date_bounds[2])
    applied_filters(default_filters)
  })
  filtered <- reactive({
    validate(need(is.null(source_error), "Les données ne sont pas disponibles. Consultez le message en haut de page."))
    filter_signalconso(data, applied_filters())
  })
  nonempty <- reactive({
    current <- filtered()
    validate(need(nrow(current) > 0, "Aucun signalement pour cette sélection. Élargissez la période ou réinitialisez les filtres."))
    current
  })
  output$source_badge <- renderUI({
    span(class = paste("source-badge", source_info$mode), span(class = "status-dot"),
      if (source_info$mode == "demo") "Mode démonstration" else source_info$label)
  })
  output$source_notice <- renderUI({
    if (!is.null(source_error)) {
      div(class = "source-notice error-notice", role = "alert", bsicons::bs_icon("exclamation-triangle"),
        div(strong("Impossible de charger les données"), p(source_error),
          p("Vérifiez la source configurée puis relancez l’application.")))
    } else if (source_info$mode == "demo") {
      div(class = "source-notice", role = "status", bsicons::bs_icon("info-circle"),
        div(strong("Un aperçu pour explorer librement"), span(" · Données fictives de démonstration. Les chiffres ne décrivent pas les signalements réels.")))
    } else {
      div(class = "source-notice live-notice", bsicons::bs_icon("database-check"),
        span(source_info$detail))
    }
  })
  output$filter_pending <- renderUI({
    if (!isTRUE(all.equal(pending_filters(), applied_filters(), check.attributes = FALSE))) {
      span(class = "pending-badge", role = "status", "Filtres modifiés · Cliquez sur Appliquer")
    }
  })
  output$filter_summary <- renderUI({
    current <- filtered()
    filters <- applied_filters()
    selected <- unlist(filters[unname(filter_columns)], use.names = FALSE)
    div(class = "scope-summary",
      bsicons::bs_icon("funnel"), strong(paste(format_count(nrow(current)), "signalements")),
      span(class = "scope-separator", "·"),
      span(if (all(!is.na(filters$dates))) paste(format(filters$dates[1], "%d/%m/%Y"), "—", format(filters$dates[2], "%d/%m/%Y")) else "Aucune période disponible"),
      if (!length(selected)) span(class = "filter-chip", "Tous les territoires · Toutes les catégories")
      else lapply(selected, function(x) span(class = "filter-chip", x)))
  })
  output$kpi_cards <- renderUI({
    current <- filtered()
    total <- nrow(current)
    answered <- if (total) sum(current$signalement_traitement == "Signalement répondu") / total * 100 else NA_real_
    categories <- if (total) current[, .N, by = category][order(-N)] else NULL
    territories <- unique(current$dep_code[!is.na(current$dep_code) & !current$dep_code %in% c("", "Non renseigné")])
    div(class = "kpi-grid",
      metric_card("Signalements", format_count(total), "dans votre sélection", "chat-left-text", "primary"),
      metric_card("Avec une réponse", if (is.na(answered)) "—" else paste0(format(round(answered, 1), decimal.mark = ",", nsmall = 1), " %"),
        "part des signalements sélectionnés", "reply", "mint"),
      metric_card("Première catégorie", if (total) categories$category[1] else "—",
        if (total) paste0(format_count(categories$N[1]), " signalements · ", round(categories$N[1] / total * 100), " % du total") else "aucun signalement", "bookmark", "sand"),
      metric_card("Départements", format_count(length(territories)), "avec au moins un signalement", "geo-alt", "blue"))
  })
  output$exploration_timegraph <- highcharter::renderHighchart({
    graph_timeline(nonempty(), input$time_granularity %||% "month", date_range = applied_filters()$dates)
  })
  output$highchart_stats_categories <- highcharter::renderHighchart({
    graph_explore(nonempty()[, .N, by = category], "bar", input$highchart_stats_pct %||% "niv", max_items = 10)
  })
  output$highchart_stats_sigstate <- highcharter::renderHighchart({
    graph_explore(nonempty()[, .N, by = signalement_traitement], "bar", "niv")
  })
  output$highchart_stats_tags <- highcharter::renderHighchart({
    graph_explore(nonempty()[, .N, by = tags], input$highchart_stats_type %||% "bar", input$highchart_stats_pct %||% "niv")
  })
  output$exploration_seasonality <- highcharter::renderHighchart({
    graph_seasonality(nonempty(), input$select_seasonal %||% "weekday")
  })
  output$territory_map <- highcharter::renderHighchart({
    current <- nonempty()
    tryCatch(graph_territory(current, input$territory_level %||% "region"),
      error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$territory_ranking <- highcharter::renderHighchart({
    column <- if (identical(input$territory_level, "department")) "dep_name" else "reg_name"
    graph_explore(nonempty()[, .N, by = column], "bar", "niv")
  })
  observeEvent(list(input$variables_compare, applied_filters()), {
    req(input$variables_compare)
    current <- filtered()
    choices <- sort(unique(current[[input$variables_compare]]))
    selected <- intersect(isolate(input$modalites_compare), choices)
    if (!length(selected)) selected <- head(choices, 3)
    shinyWidgets::updatePickerInput(session, "modalites_compare", choices = choices, selected = selected)
  }, ignoreNULL = FALSE)
  output$comparison_chart <- highcharter::renderHighchart({
    current <- nonempty()
    validate(need(length(input$modalites_compare) >= 2, "Choisissez au moins deux groupes à comparer."),
      need(length(input$modalites_compare) <= 5, "Sélectionnez au maximum cinq groupes pour garder le graphique lisible."))
    graph_compare(current, input$compare_dimension %||% "category", input$variables_compare,
      input$modalites_compare, input$highchart_compare_pct %||% "percent")
  })
  output$exploration_donnees_brutes <- DT::renderDT({
    current <- filtered()
    DT::datatable(current[, .(date, category, subcategories, dep_name, reg_name, signalement_traitement, tags)],
      colnames = c("Date", "Catégorie", "Sous-catégorie", "Département", "Région", "Traitement", "Étiquettes"),
      rownames = FALSE, selection = "none", escape = TRUE,
      options = list(pageLength = 15, lengthMenu = c(15, 30, 50, 100), scrollX = TRUE,
        order = list(list(0, "desc")), language = dt_french()))
  }, server = TRUE)
  output$downloadData <- downloadHandler(
    filename = function() paste0("signalconso-", if (source_info$mode == "demo") "DEMONSTRATION-" else "", Sys.Date(), ".csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      export <- copy(filtered())
      export[, source_donnees := if (source_info$mode == "demo") "DEMONSTRATION - DONNEES FICTIVES" else source_info$label]
      # Neutralize spreadsheet formulas in imported labels before CSV export.
      text_cols <- names(export)[vapply(export, is.character, logical(1))]
      for (column in text_cols) {
        values <- export[[column]]
        unsafe <- !is.na(values) & grepl("^[[:space:]]*[=+@-]", values)
        values[unsafe] <- paste0("'", values[unsafe])
        set(export, j = column, value = values)
      }
      data.table::fwrite(export, file, sep = ";", bom = TRUE, dateTimeAs = "ISO")
    })
  observeEvent(input$methodology, {
    showModal(modalDialog(title = "Quelques clés pour lire les données", size = "l", easyClose = TRUE,
      includeMarkdown(app_sys("app/www/signalconsometh.md")),
      footer = tagList(tags$a("Consulter la source ↗", href = "https://www.data.gouv.fr/datasets/signalconso",
        target = "_blank", rel = "noopener noreferrer", class = "btn btn-outline-primary"), modalButton("J’ai compris"))))
  })
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

#' @noRd
format_count <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)

#' @noRd
metric_card <- function(label, value, detail, icon, tone) {
  div(class = paste("metric-card", paste0("metric-", tone)),
    div(class = "metric-label", span(label), span(class = "metric-icon", bsicons::bs_icon(icon))),
    div(class = paste("metric-value", if (label == "Première catégorie") "metric-category"), value),
    div(class = "metric-detail", detail))
}

#' Filter a normalized table without modifying it
#' @noRd
filter_signalconso <- function(data, filters) {
  if (is.null(data)) return(data.table::data.table())
  result <- data
  if (!is.null(filters$dates) && length(filters$dates) == 2L && !anyNA(filters$dates)) {
    result <- result[date >= filters$dates[1] & date <= filters$dates[2]]
  }
  for (column in intersect(setdiff(names(filters), "dates"), names(result))) {
    if (length(filters[[column]])) result <- result[get(column) %in% filters[[column]]]
  }
  result[]
}

#' @noRd
dt_french <- function() {
  list(search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes", thousands = " ", decimal = ",",
    info = "_START_ à _END_ sur _TOTAL_ signalements", infoEmpty = "Aucun signalement",
    infoFiltered = "(sur _MAX_ dans la sélection)", zeroRecords = "Aucun résultat pour cette recherche",
    emptyTable = "Aucun signalement pour ces filtres", processing = "Chargement…",
    paginate = list(first = "Début", previous = "Précédent", "next" = "Suivant", last = "Fin"),
    aria = list(sortAscending = " : trier par ordre croissant", sortDescending = " : trier par ordre décroissant"))
}
