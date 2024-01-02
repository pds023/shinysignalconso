#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  data <- as.data.table(read_parquet("data/signalconso.parquet"))
  list_tags <- as.data.table(read_parquet("data/list_tags.parquet"))

  colnames(data)
  data[,.N,]


}
