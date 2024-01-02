

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
                                    navset_card_underline(height = "600px",
                                                          title = "Éléments descriptifs",
                                                          nav_panel(title = "Catégories",
                                                                    highchartOutput("highchart_stats_categories"),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_categories_treemap",
                                                                      onLabel = "Bars",
                                                                      offLabel = "Treemap",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_categories_pct",
                                                                      onLabel = "N",
                                                                      offLabel = "%",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    )),
                                                          nav_panel(title = "Tags",
                                                                    highchartOutput("highchart_stats_tags"),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_tags_treemap",
                                                                      onLabel = "Bars",
                                                                      offLabel = "Treemap",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_tags_pct",
                                                                      onLabel = "N",
                                                                      offLabel = "%",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    )),
                                                          nav_panel(title = "Territoire",
                                                                    highchartOutput("highchart_stats_territoire"),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_territoire_treemap",
                                                                      onLabel = "Bars",
                                                                      offLabel = "Treemap",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_territoire_pct",
                                                                      onLabel = "N",
                                                                      offLabel = "%",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ))
                                                          ,
                                                          nav_panel(title = "État du signalement",
                                                                    highchartOutput("highchart_stats_sigstate"),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_sigstate_treemap",
                                                                      onLabel = "Bars",
                                                                      offLabel = "Treemap",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_sigstate_pct",
                                                                      onLabel = "N",
                                                                      offLabel = "%",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ))
                                    ),
                                    card(card_header("Analyse temporelle",
                                                     tooltip(
                                                       bs_icon("info-circle"),
                                                       "Tooltip message"
                                                     )),
                                           card_body(
                                             navset_card_underline(nav_panel(title = "Données brutes",
                                                                             withSpinner(highchartOutput("exploration_timegraph"))),
                                                                   nav_panel(title = "Saisonnalité",
                                                                             pickerInput(inputId = "select_seasonal",
                                                                                         choices = c("Hebdomadaire","Mensuel",
                                                                                                     "Trimestriel","Annuel"),
                                                                                         selected = "Hebdomadaire"),
                                                                             withSpinner(plotlyOutput("exploration_timegraph_seasonal"))))
                                           )

                                    ),
                                    card(card_header("Analyse spatiale",
                                                     tooltip(
                                                       bs_icon("info-circle"),
                                                       "Tooltip message"
                                                     )),
                                         card_body(
                                           navset_card_underline(nav_panel(title = "Départements",
                                                                           withSpinner(highchartOutput("exploration_map_dep"))),
                                                                 nav_panel(title = "Régions",
                                                                           withSpinner(highchartOutput("exploration_map_reg"))))
                                         )

                                    )
                          ),
                          nav_panel("Comparaisons",icon = bs_icon("graph-up"),
                                    layout_sidebar(
                                      fillable = TRUE,
                                      sidebar = sidebar(
                                        pickerInput(inputId = "variables_compare",
                                                    label = "Variable à comparer",
                                                    choices = c(""),
                                                    options = list(
                                                      `live-search` = TRUE,
                                                      `container` = 'body')),
                                        pickerInput(inputId = "modalites_compare",
                                                    label = "Modalitées",
                                                    choices = c(),
                                                    options = list(
                                                      `live-search` = TRUE,
                                                      `dropupAuto` = FALSE,
                                                      `container` = 'body'),
                                                    multiple = TRUE)
                                      ),
                                      navset_card_underline(
                                        nav_panel(title = "Catégories",
                                                  highchartOutput("highchart_compare_categories"),
                                                  switchInput(
                                                    inputId = "highchart_compare_categories_treemap",
                                                    onLabel = "Bars",
                                                    offLabel = "Treemap",
                                                    value = TRUE,
                                                    size = "mini"
                                                  ),
                                                  switchInput(
                                                    inputId = "highchart_compare_categories_pct",
                                                    onLabel = "N",
                                                    offLabel = "%",
                                                    value = TRUE,
                                                    size = "mini"
                                                  )),
                                        nav_panel(title = "Tags",
                                                  highchartOutput("highchart_compare_tags"),
                                                  switchInput(
                                                    inputId = "highchart_compare_tags_treemap",
                                                    onLabel = "Bars",
                                                    offLabel = "Treemap",
                                                    value = TRUE,
                                                    size = "mini"
                                                  ),
                                                  switchInput(
                                                    inputId = "highchart_compare_tags_pct",
                                                    onLabel = "N",
                                                    offLabel = "%",
                                                    value = TRUE,
                                                    size = "mini"
                                                  )),
                                        nav_panel(title = "Territoire",
                                                  highchartOutput("highchart_compare_territoire"),
                                                  switchInput(
                                                    inputId = "highchart_compare_territoire_treemap",
                                                    onLabel = "Bars",
                                                    offLabel = "Treemap",
                                                    value = TRUE,
                                                    size = "mini"
                                                  ),
                                                  switchInput(
                                                    inputId = "highchart_compare_territoire_pct",
                                                    onLabel = "N",
                                                    offLabel = "%",
                                                    value = TRUE,
                                                    size = "mini"
                                                  ))
                                        ,
                                        nav_panel(title = "État du signalement",
                                                  highchartOutput("highchart_compare_sigstate"),
                                                  switchInput(
                                                    inputId = "highchart_compare_sigstate_treemap",
                                                    onLabel = "Bars",
                                                    offLabel = "Treemap",
                                                    value = TRUE,
                                                    size = "mini"
                                                  ),
                                                  switchInput(
                                                    inputId = "highchart_compare_sigstate_pct",
                                                    onLabel = "N",
                                                    offLabel = "%",
                                                    value = TRUE,
                                                    size = "mini"
                                                  )))
                                    ))
              )
    )
  )
}
