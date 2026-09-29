# Run from the project root: Rscript tests/data-checks.R
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) Sys.setlocale("LC_CTYPE", ".UTF-8")
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .libPaths()))
if (file.exists("R/data_source.R")) {
  source("R/data_source.R", encoding = "UTF-8")
  source("R/preprocess_data.R", encoding = "UTF-8")
  source("R/maj_data.R", encoding = "UTF-8")
} else {
  for (name in c("load_signalconso_data", "normalize_signalconso_data", "preprocess_data", "maj_data")) {
    assign(name, get(name, asNamespace("shinySignalConso")))
  }
}

expect_error <- function(expression, text) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(text, conditionMessage(error), fixed = TRUE))
}

original_env <- Sys.getenv(c("SIGNALCONSO_DATA", "SIGNALCONSO_S3_BUCKET"), unset = NA_character_)
Sys.unsetenv(names(original_env))
set.seed(41)
previous_seed <- .Random.seed
demo <- load_signalconso_data()
stopifnot(demo$mode == "demo", nrow(demo$data) == 18000L, identical(.Random.seed, previous_seed),
          identical(demo$data, load_signalconso_data()$data),
          identical(sort(unique(demo$data$annee)), c("2023", "2024", "2025")),
          all(!is.na(demo$data$date)))

fixture <- data.frame(creationdate = c("2024-01-01T12:30:00Z", "29/02/2024", "2025-03-01"),
                      category = c("AchatInternet", NA, "Café/Restaurant"),
                      dep_code = c("1", "2A", NA), signalement_transmis = c(1, 1, NA),
                      signalement_lu = c(0, 1, NA), signalement_reponse = c(1, 0, NA))
normalized <- normalize_signalconso_data(fixture)
stopifnot(nrow(normalized) == 3L, inherits(normalized$date, "Date"),
          normalized$category[1] == "E-commerce", normalized$category[2] == "Non renseigné",
          identical(normalized$dep_code, c("01", "2A", "Non renseigné")),
          identical(normalized$signalement_traitement, c("Signalement répondu", "Signalement lu", "Non renseigné")),
          normalized$`hc-key`[1] == "fr-ara-ai", normalized$reg_code[1] == "84")
stopifnot(nrow(normalize_signalconso_data(data.frame())) == 0L)
expect_error(normalize_signalconso_data(data.frame(category = "test")), "creationdate")
expect_error(normalize_signalconso_data(data.frame(creationdate = "2024-02-30")), "invalide")
expect_error(normalize_signalconso_data(data.frame(creationdate = NA_character_)), "manquante")

temporary <- tempfile(fileext = ".csv")
output <- tempfile(fileext = ".rds")
data.table::fwrite(fixture, temporary, sep = ";")
Sys.setenv(SIGNALCONSO_DATA = temporary)
local <- load_signalconso_data()
stopifnot(local$mode == "local", nrow(local$data) == 3L)
invisible(preprocess_data(temporary, output = output))
stopifnot(file.exists(temporary), file.exists(output), nrow(readRDS(output)) == 3L)
expect_error(preprocess_data(temporary, output = temporary), "différent")
Sys.setenv(SIGNALCONSO_DATA = paste0(temporary, ".missing"))
expect_error(load_signalconso_data(), "introuvable")
Sys.unsetenv("SIGNALCONSO_DATA")
Sys.setenv(SIGNALCONSO_S3_BUCKET = "explicit-test-bucket")
if (!requireNamespace("aws.s3", quietly = TRUE)) expect_error(load_signalconso_data(), "aws.s3")

destination <- tempfile(fileext = ".csv")
file_url <- paste0("file://", if (.Platform$OS.type == "windows") "/" else "", normalizePath(temporary, winslash = "/"))
original_timeout <- getOption("timeout")
downloaded <- maj_data(url = file_url, destination = destination)
stopifnot(nrow(downloaded) == 3L, identical(getOption("timeout"), original_timeout))
saved <- readLines(destination)
writeLines("wrong_column\ninvalid", temporary)
expect_error(maj_data(url = file_url, destination = destination), "creationdate")
stopifnot(identical(readLines(destination), saved), identical(getOption("timeout"), original_timeout))

unlink(c(temporary, output, destination))
for (name in names(original_env)) {
  if (is.na(original_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(original_env[[name]]), name))
}
cat("Data checks passed: demo, dates, labels, geography, empty input, local loading, safe preprocessing and validated download.\n")
