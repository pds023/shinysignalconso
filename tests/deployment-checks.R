# Run from the project root: Rscript tests/deployment-checks.R
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .libPaths()))

check_deployment <- function() {
  manifest <- jsonlite::read_json("manifest.json", simplifyVector = FALSE)
  files <- names(manifest$files)
  packages <- names(manifest$packages)
  expected_files <- c("app.R", "DESCRIPTION", "NAMESPACE", "LICENSE", ".Rbuildignore",
    list.files(c("R", "inst"), recursive = TRUE, full.names = TRUE))
  stopifnot(
    identical(manifest$metadata$appmode, "shiny"),
    setequal(files, expected_files),
    all(c("pkgload", "shiny", "golem", "arrow", "aws.s3") %in% packages),
    !"shinySignalConso" %in% packages
  )
  expected_md5 <- vapply(manifest$files, function(file) file$checksum, character(1))
  stopifnot(identical(unname(tools::md5sum(files)), unname(expected_md5)))
  dependencies <- renv::dependencies(c("app.R", "R"), progress = FALSE)
  stopifnot(!"shinySignalConso" %in% dependencies$Package)

  # Exercise exactly the source bundle that Connect receives, from a clean cwd.
  root <- getwd()
  bundle <- tempfile("signalconso-bundle-")
  dir.create(bundle)
  bundle <- normalizePath(bundle, winslash = "/")
  stopifnot(startsWith(bundle, paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  on.exit({ setwd(root); unlink(bundle, recursive = TRUE) }, add = TRUE)
  for (file in files) {
    destination <- file.path(bundle, file)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file, destination))
  }
  if ("shinySignalConso" %in% loadedNamespaces()) pkgload::unload("shinySignalConso")
  previous_config <- Sys.getenv("R_CONFIG_ACTIVE", unset = NA_character_)
  on.exit({
    if (is.na(previous_config)) Sys.unsetenv("R_CONFIG_ACTIVE")
    else Sys.setenv(R_CONFIG_ACTIVE = previous_config)
  }, add = TRUE)
  Sys.setenv(R_CONFIG_ACTIVE = "rsconnect")
  setwd(bundle)
  app <- source("app.R", local = new.env(), encoding = "UTF-8")$value
  stopifnot(
    inherits(app, "shiny.appobj"),
    identical(normalizePath(getNamespaceInfo("shinySignalConso", "path")), normalizePath(bundle)),
    file.exists(get("app_sys", asNamespace("shinySignalConso"))("app/www/styles.css"))
  )
  html <- htmltools::renderTags(get("app_ui", asNamespace("shinySignalConso"))(NULL))$html
  stopifnot(grepl("www/logo.svg", html, fixed = TRUE))
  pkgload::unload("shinySignalConso")
  cat(sprintf("Deployment checks passed: %d files, %d packages, isolated golem startup.\n",
    length(files), length(packages)))
}

check_deployment()
