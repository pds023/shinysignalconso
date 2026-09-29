#' Analysis views
#' @return A bslib tab set containing overview, territory, comparison and data views.
#' @export
nav_panel_exploration <- function() {
  bslib::navset_tab(id = "analysis_view",
    bslib::nav_panel("Vue d’ensemble", value = "overview", icon = bsicons::bs_icon("grid-1x2"),
      uiOutput("kpi_cards"),
      div(class = "overview-grid",
        section_card("L’évolution des signalements", "Prendre du recul sur les tendances dans le temps.",
          highcharter::highchartOutput("exploration_timegraph", height = "300px"),
          controls = selectInput("time_granularity", "Regroupement", c("Par mois" = "month", "Par semaine" = "week", "Par jour" = "day"), selected = "month")),
        section_card("Les catégories à la une", "Les principales catégories, en un regard.",
          highcharter::highchartOutput("highchart_stats_categories", height = "300px"),
          controls = radioButtons("highchart_stats_pct", "Unité", c("Nombre" = "niv", "%" = "percent"), inline = TRUE))),
      div(class = "overview-grid secondary-grid",
        section_card("Le parcours des signalements", "Du dépôt à la réponse du professionnel.",
          highcharter::highchartOutput("highchart_stats_sigstate", height = "260px")),
        section_card("Les rythmes de consommation", "Nombre de signalements cumulés sur la période sélectionnée.",
          highcharter::highchartOutput("exploration_seasonality", height = "260px"),
          controls = selectInput("select_seasonal", "Regroupement", c("Jour de la semaine" = "weekday", "Mois de l’année" = "month")))),
      section_card("Explorer les motifs", "Répartition des étiquettes associées aux signalements.",
        highcharter::highchartOutput("highchart_stats_tags", height = "300px"),
        controls = radioButtons("highchart_stats_type", "Représentation", c("Barres" = "bar", "Mosaïque" = "treemap"), inline = TRUE))),
    bslib::nav_panel("Territoires", value = "territories", icon = bsicons::bs_icon("geo-alt"),
      div(class = "view-intro", h2("Une lecture des territoires"), p("Repérez où se concentrent les signalements de votre sélection.")),
      section_card("La répartition géographique", "Volumes bruts · Les différences de population ne sont pas prises en compte.",
        div(class = "territory-grid",
          highcharter::highchartOutput("territory_map", height = "480px"),
          highcharter::highchartOutput("territory_ranking", height = "480px")),
        controls = radioButtons("territory_level", "Échelle", c("Régions" = "region", "Départements" = "department"), inline = TRUE)),
      div(class = "reading-note", bsicons::bs_icon("info-circle"), " Un volume élevé ne signifie pas qu’un territoire présente davantage de problèmes : il dépend aussi de sa population et du recours au service.")),
    bslib::nav_panel("Comparaisons", value = "compare", icon = bsicons::bs_icon("bar-chart"),
      div(class = "view-intro", h2("Mettre les profils en regard"), p("Comparez jusqu’à cinq groupes sur le même périmètre de données.")),
      div(class = "comparison-controls",
        create_picker("variables_compare", "Comparer par", choices = c("Année" = "annee", "Région" = "reg_name", "Catégorie" = "category", "Traitement" = "signalement_traitement"), multiple = FALSE, selected = "annee"),
        create_picker("modalites_compare", "Groupes à comparer", select_all = FALSE),
        selectInput("compare_dimension", "Répartir par", c("Catégorie" = "category", "Étiquette" = "tags", "Département" = "dep_name", "Traitement" = "signalement_traitement")),
        radioButtons("highchart_compare_pct", "Unité", c("Nombre" = "niv", "% du groupe" = "percent"), selected = "percent", inline = TRUE)),
      section_card("Des profils à comparer", "En pourcentage, chaque groupe a son propre total comme référence.",
        highcharter::highchartOutput("comparison_chart", height = "440px"))),
    bslib::nav_panel("Données", value = "data", icon = bsicons::bs_icon("table"),
      div(class = "view-intro", h2("Revenir aux données"), p("Recherchez dans les signalements du périmètre sélectionné, puis exportez-les au format CSV.")),
      section_card("Les signalements en détail", "La recherche du tableau affine l’affichage. L’export contient tout le périmètre défini par les filtres latéraux.",
        DT::DTOutput("exploration_donnees_brutes")),
      div(class = "reading-note", bsicons::bs_icon("download"), " Export CSV compatible Excel · Encodage UTF-8 · Séparateur point-virgule")))
}
#' @noRd
section_card <- function(title, subtitle, ..., controls = NULL, class = NULL) {
  tags$section(class = paste("chart-card", class),
    div(class = "chart-card-header", div(h2(title), p(subtitle)),
      if (!is.null(controls)) div(class = "chart-controls", controls)),
    div(class = "chart-card-body", ...))
}
