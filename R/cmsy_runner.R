# R/cmsy_runner.R
# Core functions to prepare and launch CMSY++ runs asynchronously

clean_engine_script <- function(engine_file, output_file) {
  if (!file.exists(engine_file)) stop("CMSY++ engine not found: ", engine_file)
  engine_code <- readLines(engine_file, warn = FALSE)
  # Remove rm(list=ls(...)) statements that clear variables
  cleaned_code <- engine_code[!grepl("^\\s*rm\\(list\\s*=\\s*ls\\(", engine_code)]
  # Also remove graphics.off() or windows() replacements if they interfere,
  # but standard graphics.off() is fine. Let's make sure it doesn't clear important variables.
  writeLines(cleaned_code, output_file)
}

run_cmsy_job <- function(
  engine_file = "engine/CMSY++16.R",
  workdir,
  catch_file,
  id_file,
  nn_file = "engine/ffnn.bin",
  params = list()
) {
  required_pkgs <- c("processx", "R2jags", "coda", "foreach", "doParallel", "gplots", "mvtnorm", "snpar",
                     "neuralnet", "conicfit", "pracma", "geigen", "LNPar")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop("Missing required R packages: ", paste(missing_pkgs, collapse = ", "))
  }
  if (!file.exists(catch_file)) stop("Catch file not found: ", catch_file)
  if (!file.exists(id_file)) stop("ID file not found: ", id_file)
  if (!file.exists(nn_file)) stop("Neural-network file not found: ", nn_file)

  # Create directory
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  
  # Read and clean engine
  cleaned_engine_path <- file.path(workdir, "engine_cleaned.R")
  clean_engine_script(engine_file, cleaned_engine_path)
  
  # Construct configuration lines
  # Logical values must be converted to R's TRUE/FALSE strings
  to_r_val <- function(val) {
    if (is.list(val)) val <- val[[1]]
    if (length(val) == 0) val <- NA
    if (length(val) == 1 && is.na(val)) return("NA")
    if (is.logical(val)) {
      return(ifelse(val, "TRUE", "FALSE"))
    } else if (is.character(val)) {
      return(paste(deparse(as.character(val)), collapse = ""))
    } else {
      return(as.character(val))
    }
  }
  
  # Define default parameters if missing
  defaults <- list(
    stocks = NA,
    n_chains = 3,
    CV_C = 0.15,
    CV_cpue = 0.20,
    sigmaR = 0.10,
    cor_log_rk = -0.76,
    n = 10000,
    parallel_chains = FALSE,
    mcmc_iter = 60000,
    mcmc_burnin = 30000,
    mcmc_thin = 10,
    bw = 3,
    nab = 3,
    bt4pr = FALSE,
    retros = TRUE,
    kobe_plot = TRUE,
    BSMfits_plot = TRUE,
    pp_plot = TRUE,
    rk_diags = TRUE,
    save_plots = TRUE,
    close_plots = TRUE,
    write_output = TRUE,
    write_pdf = FALSE,
    write_rdata = TRUE
  )
  
  # Merge custom parameters with defaults
  p <- modifyList(defaults, params)
  
  config_lines <- c(
    "# Auto-generated configuration for CMSY++ run",
    "set.seed(999)",
    sprintf("catch_file  <- %s", to_r_val(basename(catch_file))),
    sprintf("id_file     <- %s", to_r_val(basename(id_file))),
    sprintf("nn_file     <- %s", to_r_val(basename(nn_file))),
    sprintf("outfile     <- %s", to_r_val(paste0("Out_", basename(id_file)))),
    sprintf("stocks      <- %s", to_r_val(p$stocks)),
    sprintf("n.chains    <- %s", to_r_val(p$n_chains)),
    sprintf("CV.C        <- %s", to_r_val(p$CV_C)),
    sprintf("CV.cpue     <- %s", to_r_val(p$CV_cpue)),
    sprintf("sigmaR      <- %s", to_r_val(p$sigmaR)),
    sprintf("cor.log.rk  <- %s", to_r_val(p$cor_log_rk)),
    sprintf("n           <- %s", to_r_val(p$n)),
    sprintf("parallel.chains <- %s", to_r_val(p$parallel_chains)),
    sprintf("mcmc.iter   <- %s", to_r_val(p$mcmc_iter)),
    sprintf("mcmc.burnin <- %s", to_r_val(p$mcmc_burnin)),
    sprintf("mcmc.thin   <- %s", to_r_val(p$mcmc_thin)),
    sprintf("bw          <- %s", to_r_val(p$bw)),
    sprintf("nab         <- %s", to_r_val(p$nab)),
    sprintf("bt4pr       <- %s", to_r_val(p$bt4pr)),
    sprintf("retros      <- %s", to_r_val(p$retros)),
    sprintf("kobe.plot   <- %s", to_r_val(p$kobe_plot)),
    sprintf("BSMfits.plot <- %s", to_r_val(p$BSMfits_plot)),
    sprintf("pp.plot     <- %s", to_r_val(p$pp_plot)),
    sprintf("rk.diags    <- %s", to_r_val(p$rk_diags)),
    sprintf("save.plots  <- %s", to_r_val(p$save_plots)),
    sprintf("close.plots <- %s", to_r_val(p$close_plots)),
    sprintf("write.output <- %s", to_r_val(p$write_output)),
    sprintf("write.pdf   <- %s", to_r_val(p$write_pdf)),
    sprintf("write.rdata <- %s", to_r_val(p$write_rdata))
  )
  
  # Final run script content: config + cleaned engine code
  engine_lines <- readLines(cleaned_engine_path, warn = FALSE)
  run_script_lines <- c(
    "Sys.setenv(RSTUDIO='')",
    "Sys.setenv(RSTUDIO_SESSION_PORT='')",
    sprintf(".libPaths(c(%s, .libPaths()))", to_r_val(normalizePath(.libPaths()[1], mustWork = FALSE))),
    config_lines, 
    "", 
    engine_lines
  )
  
  run_script_path <- file.path(workdir, "run_CMSY_configured.R")
  writeLines(run_script_lines, run_script_path)
  
  # Copy files to run directory (skip if already there)
  safe_copy <- function(from, to_dir) {
    if (!file.exists(from)) {
      warning(paste("Source file not found:", from))
      stop("Source file not found: ", from)
    }
    dest <- file.path(to_dir, basename(from))
    if (normalizePath(from, mustWork = FALSE) != normalizePath(dest, mustWork = FALSE)) {
      copied <- file.copy(from, dest, overwrite = TRUE)
      if (!isTRUE(copied)) stop("Failed to copy ", from, " to ", dest)
    }
  }
  safe_copy(catch_file, workdir)
  safe_copy(id_file, workdir)
  safe_copy(nn_file, workdir)
  
  # Create a blank log file
  log_file <- file.path(workdir, "cmsy_run_log.txt")
  writeLines(character(0), log_file)
  
  # Launch process asynchronously using processx
  r_bin <- file.path(R.home("bin"), "Rscript")
  if (.Platform$OS.type == "windows") {
    r_bin <- paste0(r_bin, ".exe")
  }
  proc <- processx::process$new(
    command = r_bin,
    args = "run_CMSY_configured.R",
    wd = normalizePath(workdir),
    stdout = normalizePath(log_file, mustWork = FALSE),
    stderr = "2>&1"
  )
  
  return(list(
    proc = proc,
    workdir = workdir,
    log_file = log_file,
    script_file = run_script_path
  ))
}
