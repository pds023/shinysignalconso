

#' Title
#'
#' @param data
#' @param length
#'
#' @return
#' @export
#'
#' @examples
create_dt <- function(data,length = 5,cols_names = NULL,select_cols = FALSE) {
  if(select_cols){
    initial_visibility <- c(TRUE, TRUE, rep(FALSE, length(colnames(data)) - 2))
    return(
      renderDT(
        data,
        filter = 'top',
        selection = 'none',
        extensions = c('Buttons'),
        colnames = colnames(data),
        rownames = FALSE,
        options = list(
          pageLength = length,
          scrollX = TRUE,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.13.7/i18n/fr-FR.json'
          ),
          dom = 'Bfrtip',
          buttons = "colvis",
          columnDefs = list(
            list(visible = TRUE, targets = c(0:5)),  # Affiche uniquement les deux premières colonnes
            list(visible = FALSE, targets = c(6:length(colnames(data)) - 1))  # Cache toutes les autres colonnes
          )
        )))
  } else{
    return(
      renderDT(
        data,
        filter = 'top',
        extensions = c('Buttons'),
        colnames = cols_names,
        rownames = FALSE,
        options = list(
          pageLength = length,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.13.7/i18n/fr-FR.json'
          ),
          dom = 'Bfrtip',
          buttons = "colvis")
      ))
  }
}
