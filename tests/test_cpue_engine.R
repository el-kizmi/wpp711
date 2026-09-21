.libPaths(c(normalizePath("R_library"), .libPaths()))
source("R/batch_helpers.R")
source("R/cpue_helpers.R")
source("R/cmsy_runner.R")
source("R/mod_batch_run.R")

stock <- "Katsuwonus_pelamis_711"
catch_df <- read.csv("batch_catch_pelagis.csv", stringsAsFactors = FALSE)
id_df <- read.csv("batch_id_pelagis.csv", stringsAsFactors = FALSE)
years <- sort(unique(catch_df$yr[catch_df$Stock == stock]))

set.seed(31)
raw <- do.call(rbind, lapply(seq_along(years), function(i) {
  n <- 20
  effort <- sample(1:6, n, replace = TRUE)
  gt <- sample(c(10, 20, 30), n, replace = TRUE)
  gear <- rep(c("Pancing", "Jaring"), length.out = n)
  rate <- exp(-.2 + .025 * i + ifelse(gear == "Pancing", .15, 0) + .12 * log(gt))
  data.frame(
    Stock = stock, yr = years[i], nama_alat_tangkap = gear,
    jumlah_hari_operasi = effort, gt_kapal = gt,
    berat_ikan_kg = rgamma(n, shape = 10, scale = rate * effort / 10)
  )
}))
fit <- standardize_cpue_dataset(raw)
stopifnot(isTRUE(fit$diagnostics$ready_for_bsm), nrow(fit$index) == length(years))

analysis_catch <- merge_abundance_index(catch_df, fit$index)
analysis_id <- id_df
analysis_id$btype[analysis_id$Stock == stock] <- "CPUE"
analysis_id$e.creep[analysis_id$Stock == stock] <- NA_real_
analysis_id$force.cmsy[analysis_id$Stock == stock] <- FALSE
prepared <- prepare_stock_inputs(analysis_catch, analysis_id, stock)

root <- tempfile("cpue_engine_", tmpdir = "runs")
input_dir <- file.path(root, "input")
workdir <- file.path(root, "run")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
catch_path <- file.path(input_dir, "catch.csv")
id_path <- file.path(input_dir, "id.csv")
write.csv(prepared$catch, catch_path, row.names = FALSE, na = "NA")
write.csv(prepared$id, id_path, row.names = FALSE, na = "NA")

recommended_cv <- median(fit$diagnostics$recommended_cv, na.rm = TRUE)
job <- run_cmsy_job(
  workdir = workdir, catch_file = catch_path, id_file = id_path,
  params = list(
    stocks = stock, n_chains = 2, n = 3000, CV_cpue = max(.05, recommended_cv),
    mcmc_iter = 10000, mcmc_burnin = 5000, mcmc_thin = 5,
    retros = FALSE, kobe_plot = FALSE, BSMfits_plot = FALSE,
    pp_plot = FALSE, rk_diags = FALSE, save_plots = FALSE,
    close_plots = TRUE, write_pdf = FALSE
  )
)
deadline <- Sys.time() + 120
while (job$proc$is_alive() && Sys.time() < deadline) Sys.sleep(.25)
if (job$proc$is_alive()) {
  job$proc$kill()
  stop("CPUE engine acceptance test timed out.")
}
if (!identical(job$proc$get_exit_status(), 0L)) {
  stop(paste(readLines(job$log_file, warn = FALSE), collapse = "\n"))
}
result <- parse_cmsy_output(workdir, stock)
stopifnot(!is.null(result), result$btype == "CPUE", is.finite(result$MSY), result$MSY > 0)
unlink(root, recursive = TRUE, force = TRUE)

cat("CPUE-to-cMSY++ engine acceptance test passed\n")
