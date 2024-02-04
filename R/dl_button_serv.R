
#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
dl_button_serv <- function(data,label) {
  downloadHandler(
    filename = function(){paste0(label,".csv")},
    content = function(file){write.csv(data,file,row.names = FALSE)}
  )

}
