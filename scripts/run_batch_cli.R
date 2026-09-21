#!/usr/bin/env Rscript

# Headless, reproducible CMSY++ batch runner. Usage:
# Rscript scripts/run_batch_cli.R --catch=batch_catch_pelagis.csv --id=batch_id_pelagis.csv

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[[length(hit)]])
}
as_flag <- function(x) tolower(x) %in% c("1", "true", "yes", "y")

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(root)
local_library <- normalizePath("R_library", mustWork = FALSE)
dir.create(local_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_library, .libPaths()))
source("R/batch_helpers.R")
source("R/cmsy_runner.R")
source("R/mod_batch_run.R")

catch_file <- normalizePath(arg("catch", "batch_catch_pelagis.csv"), mustWork = TRUE)
id_file <- normalizePath(arg("id", "batch_id_pelagis.csv"), mustWork = TRUE)
catch_unit <- arg("catch-unit", "kg")
max_parallel <- as.integer(arg("max-parallel", "2"))
if (!is.finite(max_parallel) || max_parallel < 1) stop("--max-parallel must be >= 1")

catch_df <- read.csv(catch_file, stringsAsFactors = FALSE, check.names = FALSE)
id_df <- read.csv(id_file, stringsAsFactors = FALSE, check.names = FALSE)
required_catch <- c("Stock", "yr", "ct")
required_id <- c("Stock", "Group", "Resilience", "btype")
if (!all(required_catch %in% names(catch_df))) stop("Missing catch columns: ", paste(setdiff(required_catch, names(catch_df)), collapse = ", "))
if (!all(required_id %in% names(id_df))) stop("Missing ID columns: ", paste(setdiff(required_id, names(id_df)), collapse = ", "))

all_stocks <- unique(c(as.character(catch_df$Stock), as.character(id_df$Stock)))
checks <- unlist(lapply(all_stocks, function(s) validate_single_stock(catch_df, id_df, s)), recursive = FALSE)
errors <- Filter(function(x) identical(x$Status, "ERROR"), checks)
if (length(errors)) {
  details <- vapply(errors, function(x) paste(x$Stock, x$Check, x$Details, sep = " | "), character(1))
  stop("Input validation failed:\n", paste(details, collapse = "\n"))
}
requested <- arg("stocks", "")
stocks <- if (nzchar(requested)) trimws(strsplit(requested, ",", fixed = TRUE)[[1]]) else all_stocks
unknown <- setdiff(stocks, all_stocks)
if (length(unknown)) stop("Unknown --stocks values: ", paste(unknown, collapse = ", "))

stamp <- format(Sys.time(), "%Y%m%d_%H%M%OS3")
default_out <- file.path("runs", gsub("[^0-9A-Za-z_-]", "", paste0("cli_", stamp)))
out_dir <- normalizePath(arg("out", default_out), mustWork = FALSE)
if (dir.exists(out_dir) && length(list.files(out_dir, all.files = TRUE, no.. = TRUE))) stop("Output directory is not empty: ", out_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

safe_names <- gsub("[^A-Za-z0-9_.-]", "_", stocks)
if (anyDuplicated(safe_names)) stop("Stock names collide after filesystem sanitization")
jobs <- setNames(vector("list", length(stocks)), stocks)
for (idx in seq_along(stocks)) {
  stock <- stocks[[idx]]
  prepared <- prepare_stock_inputs(catch_df, id_df, stock, catch_unit = catch_unit)
  workdir <- file.path(out_dir, safe_names[[idx]])
  dir.create(workdir, recursive = TRUE)
  catch_path <- file.path(workdir, "catch.csv")
  id_path <- file.path(workdir, "id.csv")
  write.csv(prepared$catch, catch_path, row.names = FALSE, na = "NA", quote = TRUE)
  write.csv(prepared$id, id_path, row.names = FALSE, na = "NA", quote = TRUE)
  jobs[[stock]] <- list(stock = stock, workdir = workdir, catch = catch_path, id = id_path,
                        status = "pending", proc = NULL, result = NULL, exit_code = NA_integer_)
}

params <- list(
  n_chains = as.integer(arg("chains", "2")),
  n = as.integer(arg("prior-draws", "5000")),
  parallel_chains = FALSE,
  mcmc_iter = as.integer(arg("mcmc-iter", "60000")),
  mcmc_burnin = as.integer(arg("mcmc-burnin", "30000")),
  mcmc_thin = as.integer(arg("mcmc-thin", "10")),
  retros = as_flag(arg("retros", "true")),
  kobe_plot = as_flag(arg("plots", "false")),
  BSMfits_plot = as_flag(arg("plots", "false")),
  pp_plot = as_flag(arg("plots", "false")),
  rk_diags = as_flag(arg("plots", "false")),
  save_plots = as_flag(arg("plots", "false")),
  close_plots = TRUE,
  write_output = TRUE,
  write_pdf = FALSE,
  write_rdata = TRUE
)
if (params$mcmc_burnin >= params$mcmc_iter) stop("mcmc-burnin must be smaller than mcmc-iter")

cat("CMSY++ batch:", length(jobs), "stocks; output:", out_dir, "\n")
repeat {
  running <- names(Filter(function(x) identical(x$status, "running"), jobs))
  pending <- names(Filter(function(x) identical(x$status, "pending"), jobs))
  while (length(running) < max_parallel && length(pending)) {
    stock <- pending[[1]]
    p <- modifyList(params, list(stocks = stock))
    launched <- tryCatch(run_cmsy_job("engine/CMSY++16.R", jobs[[stock]]$workdir,
                                      jobs[[stock]]$catch, jobs[[stock]]$id,
                                      "engine/ffnn.bin", p), error = identity)
    if (inherits(launched, "error")) {
      jobs[[stock]]$status <- "error"
      writeLines(conditionMessage(launched), file.path(jobs[[stock]]$workdir, "launch_error.txt"))
    } else {
      jobs[[stock]]$status <- "running"
      jobs[[stock]]$proc <- launched$proc
      cat("START", stock, "\n")
    }
    running <- names(Filter(function(x) identical(x$status, "running"), jobs))
    pending <- names(Filter(function(x) identical(x$status, "pending"), jobs))
  }

  for (stock in running) {
    proc <- jobs[[stock]]$proc
    if (!proc$is_alive()) {
      jobs[[stock]]$exit_code <- proc$get_exit_status()
      result <- if (identical(jobs[[stock]]$exit_code, 0L))
        tryCatch(parse_cmsy_output(jobs[[stock]]$workdir, stock), error = function(e) NULL) else NULL
      if (is.null(result)) {
        jobs[[stock]]$status <- "error"
        cat("ERROR", stock, "exit", jobs[[stock]]$exit_code, "\n")
      } else {
        meta <- id_df[id_df$Stock == stock, , drop = FALSE]
        result$resilience <- as.character(meta$Resilience[1])
        jobs[[stock]]$result <- result
        jobs[[stock]]$status <- "done"
        cat("DONE ", stock, "\n")
      }
      jobs[[stock]]$proc <- NULL
    }
  }
  statuses <- vapply(jobs, `[[`, character(1), "status")
  if (all(statuses %in% c("done", "error"))) break
  Sys.sleep(0.5)
}

status_df <- data.frame(Stock = names(jobs), Status = vapply(jobs, `[[`, character(1), "status"),
                        ExitCode = vapply(jobs, function(x) x$exit_code, integer(1)), stringsAsFactors = FALSE)
write.csv(status_df, file.path(out_dir, "batch_status.csv"), row.names = FALSE)
results <- lapply(jobs, `[[`, "result")
results <- Filter(Negate(is.null), results)
if (length(results)) write.csv(build_species_table(results), file.path(out_dir, "batch_results.csv"), row.names = FALSE)
cat("SUMMARY done=", sum(status_df$Status == "done"), " error=", sum(status_df$Status == "error"), "\n", sep = "")
cat("OUTPUT_DIR=", out_dir, "\n", sep = "")
if (any(status_df$Status == "error")) quit(status = 1)
