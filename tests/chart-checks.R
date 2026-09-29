# Run from the project root: Rscript tests/chart-checks.R
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", "French_France.utf8"))
if (dir.exists(".r-library")) .libPaths(c(normalizePath(".r-library"), .libPaths()))
if (file.exists("R/chart_helpers.R")) {
  for (file in c("chart_helpers.R", "graph_explore.R", "graph_compare.R")) {
    source(file.path("R", file), encoding = "UTF-8")
  }
} else {
  for (name in c("graph_explore", "graph_compare", "graph_timeline", "graph_seasonality", "graph_territory")) {
    assign(name, get(name, asNamespace("shinySignalConso")))
  }
}
series <- function(chart, i = 1L) chart$x$hc_opts$series[[i]]$data
distribution <- data.table::data.table(category = c("A", "B"), N = c(3L, 1L))
before <- data.table::copy(distribution)
stopifnot(identical(vapply(series(graph_explore(distribution, "bar", "percent")), `[[`, numeric(1), "y"), c(75, 25)),
          identical(before, distribution))
literal <- series(graph_explore(data.frame(category = "<b>A & B</b>", N = 1)))[[1]]$name
stopifnot(identical(literal, "&lt;b&gt;A &amp; B&lt;/b&gt;"))
many <- data.frame(category = letters[1:20], N = 20:1)
combined <- series(graph_explore(many, "treemap", "percent", max_items = 10L))
stopifnot(length(combined) == 10L, sum(vapply(combined, `[[`, numeric(1), "share")) == 100,
          sum(vapply(combined, `[[`, numeric(1), "value")) == sum(many$N))
reports <- data.table::data.table(category = c("A", "A", "A", "B", "A", "B"),
                                 cohort = c(rep("X", 4), rep("Y", 2)))
before <- data.table::copy(reports)
comparison <- graph_compare(reports, "category", "cohort", c("X", "Y"), "percent")
stopifnot(identical(vapply(series(comparison), `[[`, numeric(1), "y"), c(75, 25)),
          identical(vapply(series(comparison, 2), `[[`, numeric(1), "y"), c(50, 50)),
          identical(before, reports))
literal_comparison <- graph_compare(data.frame(category = "<A>", cohort = "<B>"), "category", "cohort", "<B>")
stopifnot(identical(literal_comparison$x$hc_opts$series[[1]]$name, "&lt;B&gt;"),
          identical(literal_comparison$x$hc_opts$xAxis$categories, list("&lt;A&gt;")))
single_category <- data.frame(category = "E-commerce", cohort = "2025", N = 1L)
for (chart in list(graph_explore(single_category[c("category", "N")]),
                   graph_compare(single_category, "category", "cohort", "2025"))) {
  serialized <- jsonlite::fromJSON(jsonlite::toJSON(chart$x$hc_opts, auto_unbox = TRUE), simplifyVector = FALSE)
  stopifnot(identical(serialized$xAxis$categories, list("E-commerce")))
}
tail_data <- data.frame(category = letters[1:20], cohort = "X")
limited <- graph_compare(tail_data, "category", "cohort", "X", "percent")
stopifnot(length(series(limited)) == 12L, sum(vapply(series(limited), `[[`, numeric(1), "y")) == 60)
dates <- data.frame(date = as.Date(c("2026-01-01", "2026-01-03", "2026-03-01")))
daily <- series(graph_timeline(dates, "day"))
monthly <- series(graph_timeline(dates, "month"))
stopifnot(length(daily) == 60L, daily[[2]][[2]] == 0,
          identical(vapply(monthly, `[[`, numeric(1), 2L), c(2, 0, 1)))
extended <- series(graph_timeline(dates, "month", as.Date(c("2025-12-01", "2026-04-30"))))
stopifnot(identical(vapply(extended, `[[`, numeric(1), 2L), c(0, 2, 0, 1, 0)))
clipped <- series(graph_timeline(dates, "day", as.Date(c("2026-01-02", "2026-01-04"))))
stopifnot(identical(vapply(clipped, `[[`, numeric(1), 2L), c(0, 1, 0)))
averages <- graph_timeline(data.frame(date = as.Date("2026-01-01") + 0:90), "day")
stopifnot(length(averages$x$hc_opts$series) == 3L, is.null(series(averages, 2)[[29]][[2]]),
          abs(series(averages, 2)[[30]][[2]] - 1) < 1e-12)
stopifnot(sum(unlist(series(graph_seasonality(dates)))) == 3L,
          is.null(graph_timeline(data.frame(date = as.Date(character())))),
          is.null(graph_explore(data.frame(category = character(), N = integer()))))
territories <- data.frame(reg_code = c("11", "84", "01", NA),
                          dep_code = c("75", "69", "971", NA), N = c(3, 7, 2, 1))
for (level in c("region", "department")) {
  chart <- graph_territory(territories, level)
  stopifnot(sum(vapply(series(chart), `[[`, numeric(1), "value")) == 12,
            grepl("1 signalements hors carte", chart$x$hc_opts$subtitle$text, fixed = TRUE))
  features <- chart$x$hc_opts$series[[1]]$mapData$features
  codes <- vapply(features, function(x) x$properties$code, character(1))
  stopifnot(!anyNA(codes), !anyDuplicated(codes), length(codes) == if (level == "region") 18L else 101L)
  if (level == "department") stopifnot(all(c("75", "69", "2A", "2B", "971", "976") %in% codes))
}
cat("Chart checks passed: percentages, grouping, input immutability, time gaps, moving averages and geographic joins.\n")
