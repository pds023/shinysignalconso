#' Title
#'
#' @return
#' @export
#'
#' @examples
create_picker <- function(id, label = NULL, choices = c(), multiple = TRUE, selected = NULL,
                          select_all = TRUE) {
  if(select_all){
    pickerInput(inputId = id, label = label,choices = choices,multiple = multiple,selected = selected,
                options = pickerOptions(noneSelectedText = "Aucune sélection",
                                        liveSearch = TRUE,
                                        container = "body",
                                        actionsBox = select_all,
                                        size = 5,
                                        deselectAllText = "Tout déselectionner",
                                        liveSearchNormalize = TRUE,
                                        noneResultsText = "Aucun résultat",
                                        selectAllText = "Tout sélectionner"))
  } else{
    pickerInput(inputId = id, label = label,choices = choices,multiple = multiple,selected = selected,
                options = pickerOptions(noneSelectedText = "Aucune sélection",
                                        liveSearch = TRUE,
                                        container = "body",
                                        size = 5,
                                        liveSearchNormalize = TRUE,
                                        noneResultsText = "Aucun résultat"))
  }
}
