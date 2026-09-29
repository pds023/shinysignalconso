#' Searchable multiple selection
#' @param id Shiny input identifier.
#' @param label Label displayed above the selection control.
#' @param choices Available values, optionally named with display labels.
#' @param multiple Whether more than one value can be selected.
#' @param selected Initially selected values.
#' @param select_all Whether to show select-all and clear-all actions.
#' @return A searchable Shiny picker input.
#' @export
create_picker <- function(id, label = NULL, choices = character(), multiple = TRUE,
                          selected = NULL, select_all = TRUE) {
  shinyWidgets::pickerInput(id, label, choices, selected = selected, multiple = multiple,
    width = "100%", options = shinyWidgets::pickerOptions(
      noneSelectedText = if (multiple) "Tout inclure" else "Choisir…",
      liveSearch = TRUE, container = "body", actionsBox = multiple && select_all,
      size = 7, liveSearchNormalize = TRUE, noneResultsText = "Aucun résultat pour {0}",
      selectAllText = "Tout sélectionner", deselectAllText = "Tout effacer",
      selectedTextFormat = "count > 2", countSelectedText = "{0} sélections"
    ))
}
