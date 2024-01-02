#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_exploration <- function() {
  renderUI({
    accordion(accordion_panel(title = "Filtrer",icon = icon("filter"),
                              pickerInput(inputId = "exploration_filter_category",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'),
                                          label = "Catégorie",
                                          choices = c("")),
                              pickerInput(inputId = "exploration_filter_subcategories",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'),
                                          label = "Sous-catégorie",
                                          choices = c("")),
                              pickerInput(inputId = "exploration_filter_tags",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'), label = "Tags",
                                          choices = c("")),
                              pickerInput(inputId = "exploration_filter_region",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'), label = "Région",
                                          choices = c("")),
                              pickerInput(inputId = "exploration_filter_departement",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'), label = "Département",
                                          choices = c("")),
                              pickerInput(inputId = "exploration_filter_sigstate",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'), label = "Statut du signalement",
                                          choices = c("")),
                              pickerInput(inputId = "exploration_filter_annee",multiple = TRUE,
                                          options = list(
                                            `live-search` = TRUE,
                                            `container` = 'body'), label = "Année",
                                          choices = c("")),
                              actionButton(inputId = "exploration_filters_apply",label = "Appliquer"),
                              actionButton(inputId = "exploration_filters_reset",label = "Réinitialiser")
    ))
  })
}
