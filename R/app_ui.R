#' The application User-Interface
#' @param request Internal Shiny request.
#' @noRd
app_ui <- function(request) {
  bslib::page_sidebar(
    title = NULL, window_title = get_golem_config("app_title"), lang = "fr",
    theme = bslib::bs_theme(version = 5, bg = "#f5f7f9", fg = "#192c36", primary = "#087f73",
      base_font = "Segoe UI, Arial, sans-serif", border_radius = "0.75rem",
      "grid-breakpoints" = "(xs: 0, sm: 900px, md: 992px, lg: 1200px, xl: 1400px, xxl: 1600px)"),
    fillable = FALSE, sidebar = sidebar_exploration(),
    golem_add_external_resources(),
    tags$a(href = "#main-content", class = "skip-link", "Aller au contenu"),
    tags$main(id = "main-content", tabindex = "-1",
      div(class = "topbar",
        div(class = "eyebrow", "L’OBSERVATOIRE DE LA CONSOMMATION"),
        div(class = "topbar-actions", uiOutput("source_badge", inline = TRUE),
          actionButton("methodology", "Guide de lecture", icon = bsicons::bs_icon("info-circle"), class = "btn-guide"))),
      div(class = "page-heading",
        div(div(class = "eyebrow heading-kicker", "DES DONNÉES POUR MIEUX COMPRENDRE"),
          h1("Les signalements, ", tags$span("en perspective.")),
          p("Explorez les tendances de consommation, des grandes évolutions aux réalités locales.")),
        downloadButton("downloadData", "Exporter les données", class = "btn-export")),
      uiOutput("source_notice"),
      div(class = "scope-bar", uiOutput("filter_summary"), uiOutput("filter_pending")),
      nav_panel_exploration(),
      tags$footer(class = "app-footer",
        span("SignalConso · Des données ouvertes, un regard éclairé."),
        div(tags$a("Source : data.gouv.fr", href = "https://www.data.gouv.fr/datasets/signalconso", target = "_blank", rel = "noopener noreferrer"),
          span(" · "), tags$a("Philippe Fontaine", href = "https://www.philippefontaine.eu", target = "_blank", rel = "noopener noreferrer"),
          span(" · "), tags$a("Code source", href = "https://github.com/pds023/shinysignalconso", target = "_blank", rel = "noopener noreferrer")))))
}
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  tags$head(tags$link(rel = "icon", href = "www/logo.svg", type = "image/svg+xml"),
    tags$link(rel = "stylesheet", href = "www/styles.css"),
    tags$meta(name = "application-name", content = "SignalConso"),
    tags$meta(name = "theme-color", content = "#087f73"),
    tags$meta(name = "description", content = get_golem_config("app_description")),
    tags$meta(property = "og:title", content = get_golem_config("app_title")),
    tags$meta(property = "og:description", content = get_golem_config("app_description")))
}
