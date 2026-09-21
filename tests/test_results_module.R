.libPaths(c(normalizePath("R_library"), .libPaths()))
library(shiny)
library(shinyjs)
source("R/batch_helpers.R")
source("R/mod_batch_results.R")

fixture_root <- file.path("runs", "test_results_fixture")
fixture_dir <- file.path(fixture_root, "Coryphaena_hippurus_711")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
for (suffix in c("AN", "MAN", "KOBE")) {
  writeBin(as.raw(c(0xff, 0xd8, 0xff, 0xd9)),
           file.path(fixture_dir, paste0("Coryphaena_hippurus_711_", suffix, ".jpg")))
}

result <- list(
  stock = "Coryphaena_hippurus_711", MSY = 562, lcl.MSY = 450, ucl.MSY = 700,
  r = 0.7, k = 3100, last.B_Bmsy = 0.22, lcl.last.B_Bmsy = 0.08,
  ucl.last.B_Bmsy = 0.51, last.F_Fmsy = 1.1, lcl.last.F_Fmsy = 0.2,
  ucl.last.F_Fmsy = 10, btype = "CPUE", n_years = 20,
  workdir = fixture_dir
)

testServer(
  mod_batch_results_server,
  args = list(batch_results = reactive(list(Coryphaena_hippurus_711 = result)),
              id_df = reactive(data.frame())),
  {
    session$flushReact()
    session$setInputs(sel_stock = "Coryphaena_hippurus_711")
    session$flushReact()
    session$setInputs(plot_choice = "AN")
    session$flushReact()
    expected <- "runs/test_results_fixture/Coryphaena_hippurus_711/Coryphaena_hippurus_711_AN.jpg"
    stopifnot(grepl(expected, output$active_plot_ui$html, fixed = TRUE))
    metric_text <- gsub("[[:space:]]+", " ", output$metrics_row_ui$html)
    stopifnot(grepl("95% CI", metric_text, fixed = TRUE),
              grepl("450", metric_text, fixed = TRUE), grepl("700", metric_text, fixed = TRUE))
    session$setInputs(plot_choice = "MAN")
    session$flushReact()
    stopifnot(grepl("_MAN.jpg", output$active_plot_ui$html, fixed = TRUE))
    session$setInputs(plot_choice = "KOBE")
    session$flushReact()
    stopifnot(grepl("_KOBE.jpg", output$active_plot_ui$html, fixed = TRUE))
  }
)
unlink(fixture_root, recursive = TRUE, force = TRUE)
cat("Results module interaction tests passed\n")
