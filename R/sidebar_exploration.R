#' Sidebar filters shared by every analysis view
#' @return A bslib sidebar with date, category and territory filters.
#' @export
sidebar_exploration <- function() {
  bslib::sidebar(
    id = "filters_sidebar", width = 282, open = list(desktop = "open", mobile = "closed"),
    title = "Votre exploration",
    div(class = "sidebar-brand", span(class = "brand-symbol", bsicons::bs_icon("bar-chart-line-fill")),
      div(strong("SignalConso"), span("L’OBSERVATOIRE"))),
    div(class = "sidebar-intro", h2("Votre exploration"), p("Affinez votre regard sur les données.")),
    dateRangeInput("filter_dates", "Période", start = NULL, end = NULL,
      language = "fr", format = "dd/mm/yyyy", separator = "→", width = "100%"),
    create_picker("exploration_filter_category", "Catégories", choices = character()),
    create_picker("exploration_filter_region", "Régions", choices = character()),
    tags$details(class = "advanced-filters",
      tags$summary(bsicons::bs_icon("sliders"), " Plus de filtres"),
      create_picker("exploration_filter_departement", "Départements", choices = character()),
      create_picker("exploration_filter_subcategories", "Sous-catégories", choices = character()),
      create_picker("exploration_filter_tags", "Étiquettes", choices = character()),
      create_picker("exploration_filter_sigstate", "Traitement", choices = character())),
    div(class = "filter-actions",
      actionButton("exploration_filters_apply", "Appliquer les filtres", icon = bsicons::bs_icon("arrow-right"), class = "btn-primary", width = "100%"),
      actionButton("exploration_filters_reset", "Réinitialiser", icon = bsicons::bs_icon("arrow-counterclockwise"), class = "btn-reset", width = "100%")),
    div(class = "sidebar-note", bsicons::bs_icon("intersect"),
      p("Un même périmètre pour tous vos graphiques, comparaisons et exports.")),
    div(class = "sidebar-bottom", span(class = "open-data-dot"), span("Un projet indépendant"),
      tags$a("Découvrir SignalConso ↗", href = "https://signal.conso.gouv.fr", target = "_blank", rel = "noopener noreferrer")))
}
