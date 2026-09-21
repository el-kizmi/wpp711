# R/mod_batch_run.R
# Module: Parallel batch execution with progress grid

mod_batch_run_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "batch-run",

      # Control bar
      div(class = "run-control-bar",
        div(class = "run-control-left",
          uiOutput(ns("run_btn_ui"))
        ),
        div(class = "run-control-right",
          uiOutput(ns("progress_stats_ui"))
        )
      ),

      # Progress grid
      div(class = "progress-grid-container",
        div(class = "progress-grid-title", icon("th"), "Status Eksekusi"),
        div(class = "progress-grid", uiOutput(ns("progress_grid_ui")))
      ),

      # Log console
      div(class = "log-console",
        div(class = "log-console-header",
          icon("terminal"), "Live Console",
          actionButton(ns("btn_clear_log"), NULL, icon = icon("eraser"), class = "btn-sm btn-console-clear")
        ),
        div(class = "log-console-body", verbatimTextOutput(ns("console_output")))
      )
    )
  )
}

mod_batch_run_server <- function(id, catch_df, id_df, species_list, groups, params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      runs = list(),          # list of {stock, status, proc, workdir, log_file}
      is_running = FALSE,
      completed_count = 0,
      error_count = 0,
      start_time = NULL,
      results = list(),
      poll_tick = 0
    )
    poll_observer <- NULL

    # Run/Cancel button
    output$run_btn_ui <- renderUI({
      if (rv$is_running) {
        actionButton(ns("btn_cancel"), "Stop Semua", class = "btn-danger", icon = icon("stop"))
      } else {
        actionButton(ns("btn_run_batch"), "Mulai Batch Run", class = "btn-success", icon = icon("play"))
      }
    })

    # Progress stats
    output$progress_stats_ui <- renderUI({
      rv$poll_tick
      total <- length(rv$runs)
      done <- rv$completed_count
      errs <- rv$error_count
      running <- if (total > 0) sum(vapply(rv$runs, function(r) r$status == "running", logical(1))) else 0

      elapsed_str <- ""
      if (!is.null(rv$start_time)) {
        elapsed <- as.numeric(difftime(Sys.time(), rv$start_time, units = "secs"))
        elapsed_str <- sprintf("%02d:%02d", floor(elapsed / 60), round(elapsed %% 60))
      }

      div(class = "run-stats",
        div(class = "stat-item", span(class = "stat-num", done), span(class = "stat-label", "Done")),
        div(class = "stat-item stat-running", span(class = "stat-num", running), span(class = "stat-label", "Running")),
        div(class = "stat-item stat-error", span(class = "stat-num", errs), span(class = "stat-label", "Error")),
        div(class = "stat-item", span(class = "stat-num", total), span(class = "stat-label", "Total")),
        div(class = "stat-elapsed", icon("clock"), elapsed_str)
      )
    })

    # Progress grid
    output$progress_grid_ui <- renderUI({
      if (length(rv$runs) == 0) {
        return(div(class = "empty-state", icon("inbox", class = "fa-3x"), "Belum ada eksekusi"))
      }

      cells <- lapply(rv$runs, function(r) {
        status_cls <- switch(r$status,
          "pending"  = "cell-pending",
          "running"  = "cell-running",
          "done"     = "cell-done",
          "error"    = "cell-error",
          "cell-pending"
        )
        icon_el <- switch(r$status,
          "pending"  = icon("clock"),
          "running"  = icon("spinner", class = "fa-spin"),
          "done"     = icon("check"),
          "error"    = icon("times"),
          icon("clock")
        )
        div(class = paste("progress-cell", status_cls),
          div(class = "cell-icon", icon_el),
          div(class = "cell-stock", title = r$stock, r$stock)
        )
      })

      do.call(tagList, cells)
    })

    # Console output
    output$console_output <- renderText({
      rv$poll_tick
      all_logs <- character(0)
      if (!length(rv$runs)) return("Console output akan muncul di sini...")
      statuses <- vapply(rv$runs, `[[`, character(1), "status")
      selected <- which(statuses == "running")
      if (!length(selected)) selected <- tail(which(statuses %in% c("done", "error")), 2)
      for (r in rv$runs[selected]) {
        if (!is.null(r$log_file) && file.exists(r$log_file)) {
          lines <- tryCatch(readLines(r$log_file, warn = FALSE), error = function(e) character(0))
          if (length(lines) > 0) {
            lines <- tail(lines, 35)
            all_logs <- c(all_logs, paste0("=== ", r$stock, " ==="), lines, "")
          }
        }
      }
      if (length(all_logs) == 0) return("Console output akan muncul di sini...")
      paste(tail(all_logs, 120), collapse = "\n")
    })

    # Start batch
    observeEvent(input$btn_run_batch, {
      req(catch_df(), id_df(), species_list())
      if (rv$is_running) return()

      candidate_stocks <- unique(c(as.character(catch_df()$Stock), as.character(id_df()$Stock)))
      preflight_checks <- unlist(lapply(candidate_stocks, function(stk)
        validate_single_stock(catch_df(), id_df(), stk)), recursive = FALSE)
      preflight_errors <- Filter(function(x) identical(x$Status, "ERROR"), preflight_checks)
      if (length(preflight_errors)) {
        showNotification(paste(length(preflight_errors),
                               "error validasi; batch tidak dijalankan."),
                         type = "error", duration = 10)
        return()
      }

      rv$is_running <- TRUE
      rv$completed_count <- 0
      rv$error_count <- 0
      rv$start_time <- Sys.time()
      rv$results <- list()
      rv$poll_tick <- 0

      all_stocks <- unlist(species_list())
      run_id <- paste0("batch_", format(Sys.time(), "%Y%m%d_%H%M%OS3"))
      run_id <- gsub("[^0-9A-Za-z_-]", "", run_id)
      base_dir <- file.path("runs", run_id)

      withProgress(message = "Menyiapkan batch run...", value = 0, {
        dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

        # Build run list
        run_list <- list()
        for (stk in all_stocks) {
          incProgress(1 / length(all_stocks), detail = paste("Menyiapkan", stk, "..."))
          stk_workdir <- file.path(base_dir, stk)
          dir.create(stk_workdir, recursive = TRUE, showWarnings = FALSE)

          override <- params$override_data()
          stk_row <- override[override$Stock == stk, , drop = FALSE]
          ov <- if (nrow(stk_row)) list(e_creep = stk_row$e_creep[1],
                                        force_cmsy = stk_row$force_cmsy[1]) else list()
          prepared <- prepare_stock_inputs(catch_df(), id_df(), stk, ov)
          stk_catch <- prepared$catch
          stk_id <- prepared$id

          catch_path <- file.path(stk_workdir, "catch.csv")
          id_path <- file.path(stk_workdir, "id.csv")
          write.table(stk_catch, catch_path, row.names = FALSE, sep = ",", quote = FALSE)
          write.table(stk_id, id_path, row.names = FALSE, sep = ",", quote = FALSE)

          run_list[[stk]] <- list(
            stock = stk,
            status = "pending",
            proc = NULL,
            workdir = stk_workdir,
            log_file = file.path(stk_workdir, "cmsy_run_log.txt"),
            catch_path = catch_path,
            id_path = id_path
          )
        }
        rv$runs <- run_list
      })

      # Launch runs with max_parallel limit
      max_p <- params$max_parallel()
      launch_next <- function() {
        pending <- which(sapply(rv$runs, function(r) r$status == "pending"))
        running <- which(sapply(rv$runs, function(r) r$status == "running"))

        while (length(running) < max_p && length(pending) > 0) {
          idx <- pending[1]
          stk <- names(rv$runs)[idx]

          # Get params (with override check)
          override <- params$override_data()
          stk_row <- override[override$Stock == stk, ]
          if (nrow(stk_row) == 0) stk_row <- NULL

          safe_val <- function(val, default) {
            if (is.null(val) || length(val) == 0 || is.list(val)) return(default)
            if (length(val) == 1 && is.na(val)) return(default)
            return(val)
          }

          p <- list(
            stocks = stk,
            bw = params$bw(),
            nab = params$nab(),
            bt4pr = params$bt4pr(),
            CV_C = safe_val(stk_row$CV_C, params$CV_C()),
            CV_cpue = safe_val(stk_row$CV_cpue, params$CV_cpue()),
            sigmaR = safe_val(stk_row$sigmaR, params$sigmaR()),
            n_chains = safe_val(stk_row$n_chains, params$n_chains()),
            n = safe_val(stk_row$n_iter, params$n()),
            kobe_plot = params$kobe_plot(),
            BSMfits_plot = params$BSMfits_plot(),
            pp_plot = params$pp_plot(),
            rk_diags = params$rk_diags(),
            retros = params$retros(),
            write_rdata = params$write_rdata(),
            save_plots = TRUE,
            write_output = TRUE,
            write_pdf = FALSE
          )

          job <- tryCatch(
            run_cmsy_job(
              engine_file = "engine/CMSY++16.R",
              workdir = rv$runs[[stk]]$workdir,
              catch_file = rv$runs[[stk]]$catch_path,
              id_file = rv$runs[[stk]]$id_path,
              nn_file = "engine/ffnn.bin",
              params = p
            ),
            error = function(e) {
              msg <- paste("run_cmsy_job failed:", conditionMessage(e))
              writeLines(msg, file.path(rv$runs[[stk]]$workdir, "cmsy_run_log.txt"))
              message(msg)
              NULL
            }
          )

          if (!is.null(job)) {
            rv$runs[[stk]]$status <- "running"
            rv$runs[[stk]]$proc <- job$proc
          } else {
            rv$runs[[stk]]$status <- "error"
            rv$error_count <- rv$error_count + 1
          }

          pending <- which(sapply(rv$runs, function(r) r$status == "pending"))
          running <- which(sapply(rv$runs, function(r) r$status == "running"))
        }
      }

      # Initial launch
      launch_next()

      # Poll only while a batch is active. The previous implementation kept reading
      # every log file twice per second after completion, which made Results/Summary sluggish.
      if (!is.null(poll_observer)) poll_observer$destroy()
      poll_observer <<- observe({
        if (!isolate(rv$is_running)) return()
        invalidateLater(750, session)
        isolate({
        rv$poll_tick <- rv$poll_tick + 1
        running <- which(vapply(rv$runs, function(r) r$status == "running", logical(1)))

        for (idx in running) {
          stk <- names(rv$runs)[idx]
          proc <- rv$runs[[stk]]$proc
          if (!is.null(proc) && !proc$is_alive()) {
            exit_code <- proc$get_exit_status()
            if (exit_code == 0) {
              result <- tryCatch(parse_cmsy_output(rv$runs[[stk]]$workdir, stk),
                                 error = function(e) NULL)
              if (!is.null(result)) {
                meta <- id_df()[id_df()$Stock == stk, , drop = FALSE]
                result$resilience <- as.character(meta$Resilience[1])
                rv$runs[[stk]]$status <- "done"
                rv$completed_count <- rv$completed_count + 1
                rv$results[[stk]] <- result
              } else {
                rv$runs[[stk]]$status <- "error"
                rv$error_count <- rv$error_count + 1
              }
            } else {
              rv$runs[[stk]]$status <- "error"
              rv$error_count <- rv$error_count + 1
            }
            rv$runs[[stk]]$proc <- NULL
          }
        }

        # Launch more if slots available
        launch_next()

        # Check if all done
        all_status <- vapply(rv$runs, function(r) r$status, character(1))
        if (all(all_status %in% c("done", "error"))) {
          rv$is_running <- FALSE
          n_done <- sum(all_status == "done")
          showNotification(paste("Batch selesai:", n_done, "berhasil,", rv$error_count, "error"), type = "message", duration = 10)
        }
        })
      })
    })

    # Cancel
    observeEvent(input$btn_cancel, {
      for (stk in names(rv$runs)) {
        if (rv$runs[[stk]]$status == "running" && !is.null(rv$runs[[stk]]$proc)) {
          tryCatch(rv$runs[[stk]]$proc$kill(), error = function(e) NULL)
          rv$runs[[stk]]$status <- "error"
        }
      }
      rv$is_running <- FALSE
      rv$poll_tick <- rv$poll_tick + 1
      showNotification("Semua eksekusi dihentikan.", type = "warning")
    })

    # Clear log
    observeEvent(input$btn_clear_log, {
      for (stk in names(rv$runs)) {
        if (!is.null(rv$runs[[stk]]$log_file) && file.exists(rv$runs[[stk]]$log_file)) {
          writeLines(character(0), rv$runs[[stk]]$log_file)
        }
      }
    })

    return(list(
      is_running = reactive(rv$is_running),
      results = reactive(rv$results),
      runs = reactive(rv$runs)
    ))
  })
}

# Parse CMSY output from run directory
parse_cmsy_output <- function(workdir, stock) {
  csv_files <- list.files(workdir, pattern = "^Out_.*\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) return(NULL)

  df <- tryCatch(read.csv(csv_files[1], stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Remove header row if present
  if ("Stock" %in% names(df)) {
    df <- df[df$Stock != "Stock", ]
  }

  row <- df[df$Stock == stock, , drop = FALSE]
  if (nrow(row) != 1) return(NULL)

  num <- function(name) {
    if (!name %in% names(row)) return(NA_real_)
    suppressWarnings(as.numeric(row[[name]][1]))
  }
  tonnes <- function(name) num(name) * 1000 # engine output is in '000 tonnes
  choose_finite <- function(primary, fallback) {
    value <- num(primary)
    if (length(value) == 1 && is.finite(value)) value else num(fallback)
  }
  required <- c(num("MSY"), num("last.B_Bmsy"), num("last.F_Fmsy"), num("F_msy"), num("Bmsy"))
  if (any(!is.finite(required)) || any(required <= 0)) return(NULL)
  start_year <- num("start.yr")
  end_year <- num("end.yr")

  list(
    stock        = stock,
    MSY          = tonnes("MSY"),
    lcl.MSY      = tonnes("lcl.MSY"),
    ucl.MSY      = tonnes("ucl.MSY"),
    r            = choose_finite("r_BSM", "r_CMSY"),
    k            = choose_finite("k_BSM", "k_CMSY") * 1000,
    last.B_Bmsy  = num("last.B_Bmsy"),
    lcl.last.B_Bmsy = num("lcl.last.B_Bmsy"),
    ucl.last.B_Bmsy = num("ucl.last.B_Bmsy"),
    last.F_Fmsy  = num("last.F_Fmsy"),
    lcl.last.F_Fmsy = num("lcl.last.F_Fmsy"),
    ucl.last.F_Fmsy = num("ucl.last.F_Fmsy"),
    Fmsy         = num("F_msy"),
    Bmsy         = tonnes("Bmsy"),
    q            = num("q_BSM"),
    n_years      = if (all(is.finite(c(start_year, end_year)))) end_year - start_year + 1 else NA_real_,
    resilience   = NA_character_,
    btype        = as.character(row$btype[1]),
    group        = as.character(row$Group[1]),
    csv_path     = csv_files[1],
    workdir      = workdir
  )
}
