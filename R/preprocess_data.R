#' Préparer un fichier SignalConso
#'
#' Lit un fichier CSV, RDS ou Parquet et normalise les colonnes d'analyse.
#' Le fichier source est conservé. Aucune donnée n'est envoyée sur un serveur.
#'
#' @param file Chemin du fichier source.
#' @param output Chemin facultatif d'un fichier .rds, .csv ou .parquet à créer.
#' @return Un data.table normalisé.
#' @export
preprocess_data <- function(file = "signalconso.csv", output = NULL) {
  data <- normalize_signalconso_data(read_signalconso_file(file))
  if (!is.null(output)) {
    if (identical(normalizePath(file, mustWork = FALSE), normalizePath(output, mustWork = FALSE))) {
      stop("Le fichier de sortie doit être différent du fichier source.", call. = FALSE)
    }
    switch(tolower(tools::file_ext(output)),
      rds = saveRDS(data, output),
      csv = data.table::fwrite(data, output, bom = TRUE),
      parquet = {
        if (!requireNamespace("arrow", quietly = TRUE)) stop("Le format Parquet nécessite le package R « arrow ».", call. = FALSE)
        arrow::write_parquet(data, output)
      },
      stop("Le fichier de sortie doit être au format .csv, .rds ou .parquet.", call. = FALSE)
    )
  }
  data
}
