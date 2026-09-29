#' Télécharger et préparer les données ouvertes SignalConso
#'
#' Le téléchargement est explicite et indépendant du démarrage de l'application.
#' Le fichier téléchargé est conservé et aucune donnée n'est envoyée sur S3.
#'
#' @param url URL du fichier CSV à télécharger.
#' @param destination Chemin du fichier CSV à créer.
#' @param output Chemin facultatif du fichier normalisé à créer.
#' @return Un data.table normalisé.
#' @export
maj_data <- function(url = "https://www.data.gouv.fr/fr/datasets/r/106add51-e925-4121-b0e4-81489c26a43f",
                     destination = "signalconso.csv", output = NULL) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(600, old_timeout))
  temporary <- tempfile(fileext = ".csv", tmpdir = dirname(destination))
  on.exit(unlink(temporary), add = TRUE)
  status <- utils::download.file(url, destfile = temporary, mode = "wb", quiet = TRUE)
  if (!identical(status, 0L)) stop("Le téléchargement des données a échoué.", call. = FALSE)
  data <- preprocess_data(temporary)
  if (!file.copy(temporary, destination, overwrite = TRUE)) stop("Impossible d'enregistrer le fichier téléchargé.", call. = FALSE)
  if (!is.null(output)) return(preprocess_data(destination, output = output))
  data
}
