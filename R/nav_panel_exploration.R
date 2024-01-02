

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
                                                                      inputId = "highchart_stats_categories",
                                                                      onLabel = "Bars",
                                                                      offLabel = "Treemap",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ),
                                                                    switchInput(
                                                                      inputId = "highchart_stats_categories",
                                                                      onLabel = "N",
                                                                      offLabel = "%",
                                                                      value = TRUE,
                                                                      size = "mini"
                                                                    ))))))
  )
}
