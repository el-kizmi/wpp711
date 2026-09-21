.libPaths(c(normalizePath("R_library"), .libPaths()))
library(shiny)
library(shinyjs)
library(DT)
source("R/batch_helpers.R")
source("R/mod_batch_summary.R")

make_result <- function(stock, msy, bbmsy, ffmsy) list(
  stock = stock, MSY = msy, lcl.MSY = msy * .8, ucl.MSY = msy * 1.2,
  r = .5, k = 2000, last.B_Bmsy = bbmsy,
  lcl.last.B_Bmsy = bbmsy * .7, ucl.last.B_Bmsy = bbmsy * 1.3,
  last.F_Fmsy = ffmsy, lcl.last.F_Fmsy = ffmsy * .7,
  ucl.last.F_Fmsy = ffmsy * 1.3, n_years = 12,
  resilience = "Medium", btype = "CPUE", group = "Pelagis"
)
results <- list(
  Stok_A = make_result("Stok_A", 500, .9, .8),
  Stok_B = make_result("Stok_B", 300, .45, 1.3)
)
ids <- data.frame(Stock = names(results), Group = "Pelagis", stringsAsFactors = FALSE)

testServer(
  mod_batch_summary_server,
  args = list(batch_results = reactive(results), id_df = reactive(ids),
              groups = reactive("Pelagis")),
  {
    session$setInputs(metric_choice = "MSY")
    session$flushReact()
    cards <- gsub("[[:space:]]+", " ", output$group_cards_ui$html)
    stopifnot(grepl("Pelagis", cards, fixed = TRUE), grepl("800", cards, fixed = TRUE))
    invisible(output$matrix_dt)
    invisible(output$comparison_plot)
    session$setInputs(metric_choice = "last.B_Bmsy")
    session$flushReact()
    invisible(output$comparison_plot)
    session$setInputs(metric_choice = "last.F_Fmsy")
    session$flushReact()
    invisible(output$comparison_plot)
    narrative <- gsub("[[:space:]]+", " ", output$narrative_ui$html)
    stopifnot(grepl("Ringkasan Naratif", narrative, fixed = TRUE))
  }
)

cat("Summary module interaction tests passed\n")
