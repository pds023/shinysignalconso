# Run from the project root: Rscript tests/server-checks.R
# Also works against an installed package when run outside the source tree.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .libPaths()))
if (file.exists("DESCRIPTION") && dir.exists("R")) {
  pkgload::load_all(quiet = TRUE, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
}
server <- get("app_server", asNamespace("shinySignalConso"))

run_server_checks <- function() {
  previous <- Sys.getenv(c("SIGNALCONSO_DATA", "SIGNALCONSO_S3_BUCKET"), unset = NA_character_)
  fixture_path <- tempfile(fileext = ".rds")
  on.exit({
    unlink(fixture_path)
    for (name in names(previous)) {
      if (is.na(previous[[name]])) Sys.unsetenv(name)
      else do.call(Sys.setenv, setNames(list(previous[[name]]), name))
    }
  }, add = TRUE)
  Sys.unsetenv(names(previous))
  fixture <- data.frame(
    creationdate = paste0(rep(c("2023", "2024"), each = 4), c("-01-02", "-02-03", "-03-04", "-04-05")),
    category = c("E-commerce", "E-commerce", "E-commerce", "Achat en magasin", "E-commerce", rep("Achat en magasin", 3)),
    dep_name = rep(c("Paris", "Nord"), 4), dep_code = rep(c("75", "59"), 4),
    reg_name = rep(c("Île-de-France", "Hauts-de-France"), 4), reg_code = rep(c("11", "32"), 4),
    subcategories = "Livraison", tags = c("=1+1", "+2", "@SUM(1)", "-42", "\t=5", "Ordinaire", "Sans formule", "Autre"),
    signalement_traitement = rep(c("Signalement répondu", "Signalement lu"), 4)
  )
  saveRDS(fixture, fixture_path)
  Sys.setenv(SIGNALCONSO_DATA = fixture_path)

  shiny::testServer(server, {
    session$flushReact()
    stopifnot(is.null(source_error), nrow(filtered()) == 8L, source_info$mode == "local")
    browser_dates <- as.Date(as.numeric(date_bounds), origin = "1970-01-01")
    session$setInputs(filter_dates = browser_dates, exploration_filter_category = character(),
      exploration_filter_region = character(), exploration_filter_departement = character(),
      exploration_filter_subcategories = character(), exploration_filter_tags = character(),
      exploration_filter_sigstate = character())
    stopifnot(is.null(output$filter_pending))
    initial <- data.table::copy(data)
    # Editing a control does not change the applied scope before confirmation.
    session$setInputs(exploration_filter_departement = "Paris",
      filter_dates = as.Date(c("2023-01-01", "2023-12-31")))
    stopifnot(nrow(filtered()) == 8L,
      grepl("Appliquer", output$filter_pending$html, fixed = TRUE))
    session$setInputs(exploration_filters_apply = 1)
    stopifnot(nrow(filtered()) == 2L, all(filtered()$dep_name == "Paris"),
      all(filtered()$annee == "2023"), is.null(output$filter_pending))

    exported <- data.table::fread(output$downloadData, sep = ";", encoding = "UTF-8", colClasses = "character")
    stopifnot(nrow(exported) == nrow(filtered()), all(exported$dep_name == "Paris"),
      identical(exported$date, as.character(filtered()$date)),
      all(exported$source_donnees == "Données locales"),
      identical(exported$tags, c("'=1+1", "'@SUM(1)")), identical(initial, data))
    # A reversed date range must preserve the last valid selection.
    session$setInputs(filter_dates = as.Date(c("2024-12-31", "2023-01-01")), exploration_filters_apply = 2)
    stopifnot(nrow(filtered()) == 2L)
    session$setInputs(filter_dates = date_bounds, exploration_filter_category = "Catégorie absente",
      exploration_filters_apply = 3)
    stopifnot(nrow(filtered()) == 0L)
    empty_error <- tryCatch(output$highchart_stats_categories, error = identity)
    stopifnot(inherits(empty_error, "shiny.silent.error"),
      grepl("Aucun signalement", conditionMessage(empty_error), fixed = TRUE),
      nrow(data.table::fread(output$downloadData, sep = ";")) == 0L)

    session$setInputs(exploration_filters_reset = 1)
    stopifnot(nrow(filtered()) == 8L, identical(applied_filters(), default_filters))
    # MockShinySession has no browser: acknowledge the reset input messages.
    session$setInputs(exploration_filter_category = character(),
      exploration_filter_region = character(), exploration_filter_departement = character(),
      exploration_filter_subcategories = character(), exploration_filter_tags = character(),
      exploration_filter_sigstate = character(), filter_dates = browser_dates)
    stopifnot(is.null(output$filter_pending))
    exported <- data.table::fread(output$downloadData, sep = ";", encoding = "UTF-8", colClasses = "character")
    stopifnot(identical(exported$tags[1:5], c("'=1+1", "'+2", "'@SUM(1)", "'-42", "'=5")),
      identical(initial, data))

    session$setInputs(variables_compare = "annee", modalites_compare = c("2023", "2024"),
      compare_dimension = "category", highchart_compare_pct = "percent")
    chart <- jsonlite::fromJSON(output$comparison_chart, simplifyVector = FALSE)$x$hc_opts
    points <- function(i) vapply(chart$series[[i]]$data, `[[`, numeric(1), "y")
    stopifnot(identical(chart$yAxis$title$text, "Part dans chaque groupe (%)"),
      identical(points(1), c(25, 75)), identical(points(2), c(75, 25)))
    session$setInputs(highchart_compare_pct = "niv")
    chart <- jsonlite::fromJSON(output$comparison_chart, simplifyVector = FALSE)$x$hc_opts
    stopifnot(identical(chart$yAxis$title$text, "Signalements"),
      identical(points(1), c(1, 3)), identical(points(2), c(3, 1)))
    session$setInputs(modalites_compare = "2023")
    group_error <- tryCatch(output$comparison_chart, error = identity)
    stopifnot(inherits(group_error, "shiny.silent.error"),
      grepl("au moins deux", conditionMessage(group_error), fixed = TRUE))
  })

  Sys.unsetenv("SIGNALCONSO_DATA")
  shiny::testServer(server, {
    session$flushReact()
    stopifnot(source_info$mode == "demo", grepl("démonstration", output$source_badge$html),
      grepl("fictives", output$source_notice$html))
    path <- output$downloadData
    exported <- data.table::fread(path, sep = ";", encoding = "UTF-8")
    stopifnot(grepl("DEMONSTRATION", basename(path)), nrow(exported) == nrow(filtered()),
      all(exported$source_donnees == "DEMONSTRATION - DONNEES FICTIVES"))
  })
  Sys.setenv(SIGNALCONSO_DATA = paste0(fixture_path, ".missing"))
  shiny::testServer(server, {
    session$flushReact()
    stopifnot(source_info$mode == "error", !is.null(source_error),
      grepl("Impossible de charger", output$source_notice$html, fixed = TRUE))
    source_failure <- tryCatch(filtered(), error = identity)
    stopifnot(inherits(source_failure, "shiny.silent.error"))
  })
  cat("Server checks passed: applied filters, department/date scope, invalid dates, reset, empty selection, comparison units, safe scoped CSV exports, demo provenance and source failures.\n")
}
run_server_checks()
