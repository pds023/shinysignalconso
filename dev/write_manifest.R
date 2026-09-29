# Run from the project root: Rscript dev/write_manifest.R
# Install the app's Imports plus pkgload, rsconnect, arrow and aws.s3 first.
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .libPaths()))
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  Sys.setlocale("LC_CTYPE", ".UTF-8")
}
if (identical(unname(getOption("repos")["CRAN"]), "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Ship package source and its resources. load_all() loads it on Connect;
# shinySignalConso itself must not be resolved as an external dependency.
app_files <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", ".Rbuildignore",
  list.files(c("R", "inst"), recursive = TRUE, full.names = TRUE))
stopifnot(all(file.exists(app_files)))
rsconnect::writeManifest(
  appDir = ".",
  appFiles = app_files,
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  quarto = FALSE
)
message("manifest.json regenerated. Run Rscript tests/deployment-checks.R before deployment.")
