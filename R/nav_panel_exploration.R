

#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_panel_exploration <- function() {
  return(
    nav_panel("Exploration",icon = bs_icon("search"),
              page_navbar(id = "dataPanel_exploration",
                          sidebar = uiOutput("sidebar_exploration"),
                          nav_panel("Vue d'ensemble", icon = bs_icon("clipboard2-data"),
                                    value = "panel_exploration_vuedensemble",
                                    card(full_screen = TRUE,fill = FALSE,
                                      card_title(div(class = "card-title-container",
                                                     div(class = "title-tooltip","Éléments descriptifs",
                                                         tooltip(
                                                           bs_icon("info-circle"),
                                                           "Nombre de signalements en niveau (#) et en part du total (%). Les filtres réalisés s'appliquent aux graphiques."
                                                         )),
                                                     div(class = "radio-group-buttons",
                                                         create_radio("highchart_stats_pct","pct")))),
                                      card_body(navset_card_underline(height = "600px",

                                                                      nav_panel(title = "Catégories",
                                                                                withSpinner(highchartOutput("highchart_stats_categories"))),
                                                                      nav_panel(title = "Tags",
                                                                                highchartOutput("highchart_stats_tags")),
                                                                      nav_panel(title = "Territoire",
                                                                                highchartOutput("highchart_stats_territoire")),
                                                                      nav_panel(title = "État du signalement",
                                                                                highchartOutput("highchart_stats_sigstate"))
                                      )),
                                      card_footer(create_radio("highchart_stats_type","graph"))
                                    ),
                                    card(full_screen = TRUE,fill = FALSE,
                                         card_title("Analyse temporelle",
                                                    tooltip(
                                                      bs_icon("info-circle"),
                                                      "MMn : moyenne mobile d'ordre n"
                                                    )),
                                         card_body(
                                           navset_card_underline(nav_panel(title = "Données brutes",
                                                                           withSpinner(highchartOutput("exploration_timegraph"))),
                                                                 nav_panel(title = "Saisonnalité",
                                                                           create_picker(id = "select_seasonal",
                                                                                         choices = c("Hebdomadaire","Mensuel",
                                                                                                     "Trimestriel","Annuel"),
                                                                                         selected = "Hebdomadaire",
                                                                                         multiple = FALSE),
                                                                           withSpinner(plotlyOutput("exploration_timegraph_seasonal"))))
                                         )

                                    ),
                                    card(full_screen = TRUE,fill = FALSE,
                                         card_title("Analyse spatiale",
                                                    tooltip(bs_icon("info-circle"),
                                                            "Il serait pertinent de rapporter le nombre de signalements à la population.")),
                                         card_body(
                                           navset_card_underline(nav_panel(title = "Départements",
                                                                           withSpinner(highchartOutput("exploration_map_dep"))),
                                                                 nav_panel(title = "Régions",
                                                                           withSpinner(highchartOutput("exploration_map_reg"))))
                                         )

                                    )
                          ),
                          nav_panel("Comparaisons",icon = bs_icon("graph-up"),
                                    card(full_screen = TRUE,fill = FALSE,
                                      card_title(div(class = "card-title-container",
                                                     div(class = "title-tooltip","Comparaisons",
                                                         tooltip(
                                                           bs_icon("info-circle"),
                                                           "Comparaison d'un sous-ensemble de signalements en niveau (#) ou en part du total (%). Les filtres réalisés s'appliquent aux comparaisons."
                                                         )),
                                                     div(class = "radio-group-buttons",
                                                         create_radio("highchart_compare_pct","pct")))),
                                      card_body(
                                        layout_sidebar(
                                          fillable = TRUE,
                                          sidebar = sidebar(
                                            create_picker(id = "variables_compare", label = "Variable à comparer",multiple = FALSE),
                                            create_picker(id = "modalites_compare", label = "Modalitées")
                                          ),
                                          navset_card_underline(
                                            nav_panel(title = "Catégories",
                                                      highchartOutput("highchart_compare_categories")),
                                            nav_panel(title = "Tags",
                                                      highchartOutput("highchart_compare_tags")),
                                            nav_panel(title = "Territoire",
                                                      highchartOutput("highchart_compare_territoire")),
                                            nav_panel(title = "État du signalement",
                                                      highchartOutput("highchart_compare_sigstate")))
                                        )
                                      ))
                          ),
                          nav_panel("Données brutes",icon = bs_icon("database"),
                                    card(full_screen = TRUE,fill = FALSE,
                                         card_title("Données brutes"),
                                         card_body(DTOutput("exploration_donnees_brutes")),
                                         card_footer(downloadButton(
                                                     "downloadData", "Télécharger",
                                                     class = "btn-primary rounded-0"
                                                   )))),
              )
    )
  )
}
