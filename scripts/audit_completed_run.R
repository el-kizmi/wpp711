#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: Rscript scripts/audit_completed_run.R runs/<run_id>")
run_dir <- args[[1]]
run_dir <- normalizePath(run_dir, mustWork = TRUE)
source("R/batch_helpers.R")

catch_files <- list.files(run_dir, pattern = "^catch[.]csv$", recursive = TRUE, full.names = TRUE)
id_files <- list.files(run_dir, pattern = "^id[.]csv$", recursive = TRUE, full.names = TRUE)
if (length(catch_files) == 0 || length(catch_files) != length(id_files)) stop("Incomplete run inputs")
catch <- do.call(rbind, lapply(catch_files, read.csv, stringsAsFactors = FALSE))
catch$ct <- catch$ct * 1000 # archived engine input tonnes -> original batch kg
id <- do.call(rbind, lapply(id_files, read.csv, stringsAsFactors = FALSE))
write.csv(catch[order(catch$Stock, catch$yr), ], file.path(run_dir, "input_snapshot_catch_kg.csv"), row.names = FALSE)
write.csv(id[order(id$Stock), ], file.path(run_dir, "input_snapshot_id.csv"), row.names = FALSE)

results <- read.csv(file.path(run_dir, "batch_results.csv"), stringsAsFactors = FALSE)
quality <- do.call(rbind, lapply(split(catch, catch$Stock), function(x) {
  x <- x[order(x$yr), ]
  folds <- pmax(x$ct[-1] / x$ct[-nrow(x)], x$ct[-nrow(x)] / x$ct[-1])
  log_file <- file.path(run_dir, gsub("[^A-Za-z0-9_.-]", "_", x$Stock[1]), "cmsy_run_log.txt")
  log <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
  diag <- grep("MCMC_DIAGNOSTIC", log, value = TRUE)
  rhats <- suppressWarnings(as.numeric(sub(".*max_Rhat=([0-9.]+).*", "\\1", diag)))
  neffs <- suppressWarnings(as.numeric(sub(".*min_n_eff=([0-9.]+).*", "\\1", diag)))
  res <- results[results$Stock == x$Stock[1], , drop = FALSE]
  data.frame(
    Stock = x$Stock[1], Years = nrow(x), Catch_CV = sd(x$ct) / mean(x$ct),
    Max_Adjacent_Fold = max(folds), Total_Fold = max(x$ct) / min(x$ct),
    Max_Rhat = if (any(is.finite(rhats))) max(rhats, na.rm = TRUE) else NA_real_,
    Min_Effective_N = if (any(is.finite(neffs))) min(neffs, na.rm = TRUE) else NA_real_,
    Status = res$Status[1], Confidence = res$Confidence[1],
    stringsAsFactors = FALSE
  )
}))
write.csv(quality[order(-quality$Max_Adjacent_Fold), ], file.path(run_dir, "audit_quality.csv"), row.names = FALSE)

summary <- c(
  paste0("stocks=", nrow(quality)),
  paste0("catch_rows=", nrow(catch)),
  paste0("execution_errors=", sum(read.csv(file.path(run_dir, "batch_status.csv"))$Status != "done")),
  paste0("max_Rhat=", max(quality$Max_Rhat, na.rm = TRUE)),
  paste0("stocks_Rhat_gt_1.1=", sum(quality$Max_Rhat > 1.1, na.rm = TRUE)),
  paste0("uncertain_status=", sum(quality$Confidence == "Uncertain")),
  paste0("adjacent_jump_gt_10x=", sum(quality$Max_Adjacent_Fold > 10))
)
writeLines(summary, file.path(run_dir, "audit_summary.txt"))
cat(paste(summary, collapse = "\n"), "\n")
