source("R/batch_helpers.R")
source("R/cmsy_runner.R")
source("R/mod_batch_run.R")

catch_df <- read.csv("batch_catch_pelagis.csv", stringsAsFactors = FALSE)
id_df <- read.csv("batch_id_pelagis.csv", stringsAsFactors = FALSE)
stocks <- intersect(unique(catch_df$Stock), unique(id_df$Stock))
stopifnot(length(stocks) > 0, nrow(catch_df) > length(stocks), nrow(id_df) >= length(stocks))

checks <- unlist(lapply(stocks, function(s) validate_single_stock(catch_df, id_df, s)),
                 recursive = FALSE)
stopifnot(!any(vapply(checks, `[[`, character(1), "Status") == "ERROR"))

stock <- stocks[1]
source_rows <- catch_df[catch_df$Stock == stock, , drop = FALSE]
prepared <- prepare_stock_inputs(catch_df, id_df, stock)
stopifnot(
  prepared$id$StartYear == min(source_rows$yr),
  prepared$id$EndYear == max(source_rows$yr),
  prepared$catch$ct[1] == source_rows$ct[order(source_rows$yr)][1] / 1000,
  prepared$engine_catch_unit == "tonnes"
)

# Parser fixture is generated locally so the test never depends on an archived run.
fixture_root <- file.path("runs", "test_pipeline_fixture")
dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
fixture <- data.frame(
  Stock = stock, Group = "Large Pelagics", btype = "CPUE",
  MSY = 0.562, lcl.MSY = 0.45, ucl.MSY = 0.70,
  r_BSM = 0.7, r_CMSY = 0.6, k_BSM = 3.1, k_CMSY = 2.9,
  last.B_Bmsy = 0.82, lcl.last.B_Bmsy = 0.60, ucl.last.B_Bmsy = 1.05,
  last.F_Fmsy = 0.91, lcl.last.F_Fmsy = 0.55, ucl.last.F_Fmsy = 1.30,
  F_msy = 0.35, Bmsy = 1.55, q_BSM = 0.002,
  start.yr = 2011, end.yr = 2022,
  check.names = FALSE
)
write.csv(fixture, file.path(fixture_root, paste0("Out_test_", stock, ".csv")), row.names = FALSE)
parsed <- parse_cmsy_output(fixture_root, stock)
stopifnot(
  !is.null(parsed), parsed$MSY == 562, parsed$Bmsy == 1550,
  parsed$n_years == 12, parsed$group == "Large Pelagics", parsed$btype == "CPUE"
)
unlink(fixture_root, recursive = TRUE, force = TRUE)

cat("All pipeline tests passed\n")
