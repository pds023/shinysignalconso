


#' Title
#'
#' @param data
#' @param input_var
#' @param input_mod
#' @param groupColumn
#'
#' @return
#' @export
#'
#' @examples
graph_compare <- function(data,groupColumn,input_var,input_mod,input_pct){

  if(is.null(data)) return(NULL)

  # Initialisation de la liste pour collecter les sous-ensembles de données
  data_list <- list()

  # Boucle sur chaque modalité choisie par l'utilisateur
  for(i in seq_along(input_mod)){
    if(length(input_var) > 0){
      data_subset <- data[get(input_var) %in% input_mod[i]]
      if(nrow(data_subset) > 0){
        # Calculer le nombre de cas pour chaque catégorie
        data_subset <- data_subset[,.N, by = groupColumn]
        colnames(data_subset) <- c("var", "N")

        # Ajouter la colonne pour la variable comparée
        data_subset[, var_compare := input_mod[i]]
        # Ajouter la colonne en %
        data_subset[,pct := round(N/sum(N),4)]

        # Ajouter le sous-ensemble à la liste
        data_list[[i]] <- data_subset
      }
    }
  }

  # Assembler tous les sous-ensembles de données
  if(length(data_list) > 0){
    data_graph <- rbindlist(data_list)

    # Affichage du graphique
    if(input_pct %in% "nb"){
      return(hchart(data_graph, type = "column", hcaes(x = var, y = "N", group = "var_compare")))
    } else{
      return(hchart(data_graph, type = "column", hcaes(x = var, y = "pct", group = "var_compare")))
    }

  }
}
