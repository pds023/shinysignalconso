

#' Title
#'
#' @return
#' @export
#'
#' @examples
maj_data <- function(url = "https://www.data.gouv.fr/fr/datasets/r/106add51-e925-4121-b0e4-81489c26a43f",
                     destination = "signalconso.csv") {
  source("set_cfg.R")

  # Augmentation du temps limite pour le téléchargement (en secondes)
  options(timeout = 600)  # 5 minutes, par exemple

  # Téléchargement du fichier
  download.file(url, destfile = destination, mode = "wb")

  # Réinitialisation de l'option de temps limite à sa valeur par défaut
  options(timeout = 60)

  preprocess_data()

}
