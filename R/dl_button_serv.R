
#' Create a CSV download handler
#'
#' @param data A data frame to export.
#' @param label File name without the CSV extension.
#' @return A Shiny download handler.
#' @export
dl_button_serv <- function(data,label) {
  downloadHandler(
    filename = function(){paste0(label,".csv")},
    content = function(file){write.csv(data,file,row.names = FALSE)}
  )

}
