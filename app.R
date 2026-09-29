# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  Sys.setlocale("LC_CTYPE", ".UTF-8")
}
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .libPaths()))
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
# Resolve the export after load_all(), without declaring this source package
# as an external dependency for rsconnect's static dependency discovery.
getExportedValue("shinySignalConso", "run_app")()
