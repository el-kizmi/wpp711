# Module: responsive per-stock results browser.

mod_batch_results_ui <- function(id) {
  ns <- NS(id)
  div(class = "batch-results",
    div(id = ns("empty_state"), class = "empty-state",
      icon("inbox", class = "fa-3x"),
      h3("Belum ada hasil"),
      p("Jalankan model untuk menampilkan estimasi stok.")),
    div(id = ns("results_content"), style = "display:none;",
      div(class = "results-nav",
        div(class = "results-nav-left",
          selectInput(ns("sel_stock"), "Stok", choices = NULL, width = "440px")),
        div(class = "results-nav-right",
          actionButton(ns("btn_prev"), "Sebelumnya", icon = icon("chevron-left"), class = "btn-sm btn-nav-text"),
          actionButton(ns("btn_next"), "Berikutnya", icon = icon("chevron-right"), class = "btn-sm btn-nav-text"))
      ),
      uiOutput(ns("status_hero_ui")),
      uiOutput(ns("metrics_row_ui")),
      div(class = "plot-panel",
        div(class = "plot-panel-header",
          div(
            h3("Grafik hasil"),
            p("Satu grafik ditampilkan pada satu waktu agar navigasi tetap ringan.")),
          radioButtons(ns("plot_choice"), NULL, inline = TRUE, selected = "AN",
            choices = c("Analisis" = "AN", "Pengelolaan" = "MAN", "Kobe" = "KOBE"))
        ),
        div(class = "plot-frame", uiOutput(ns("active_plot_ui")))
      )
    )
  )
}

mod_batch_results_server <- function(id, batch_results, id_df) {
  moduleServer(id, function(input, output, session) {
    current_idx <- reactiveVal(1)

    has_results <- reactive({
      results <- batch_results()
      !is.null(results) && length(results) > 0
    })

    observe({
      if (has_results()) {
        shinyjs::hide("empty_state")
        shinyjs::show("results_content")
      } else {
        shinyjs::show("empty_state")
        shinyjs::hide("results_content")
      }
    })

    stocks_key <- reactive(paste(names(batch_results()), collapse = "\r"))
    observeEvent(stocks_key(), {
      stocks <- names(batch_results())
      if (!length(stocks)) return()
      selected <- isolate(input$sel_stock)
      if (is.null(selected) || !selected %in% stocks) selected <- stocks[1]
      current_idx(match(selected, stocks))
      updateSelectInput(session, "sel_stock", choices = stocks, selected = selected)
    }, ignoreInit = FALSE)

    observeEvent(input$btn_prev, {
      stocks <- names(batch_results())
      req(length(stocks))
      idx <- match(input$sel_stock, stocks)
      if (!length(idx) || is.na(idx)) idx <- 1
      idx <- if (idx > 1) idx - 1 else length(stocks)
      current_idx(idx)
      updateSelectInput(session, "sel_stock", selected = stocks[idx])
    })

    observeEvent(input$btn_next, {
      stocks <- names(batch_results())
      req(length(stocks))
      idx <- match(input$sel_stock, stocks)
      if (!length(idx) || is.na(idx)) idx <- 1
      idx <- if (idx < length(stocks)) idx + 1 else 1
      current_idx(idx)
      updateSelectInput(session, "sel_stock", selected = stocks[idx])
    })

    observeEvent(input$sel_stock, {
      stocks <- names(batch_results())
      idx <- match(input$sel_stock, stocks)
      if (length(idx) && !is.na(idx)) current_idx(idx)
    })

    current_result <- reactive({
      req(has_results(), input$sel_stock)
      result <- batch_results()[[input$sel_stock]]
      req(result)
      result
    })

    safe_number <- function(x, digits = 2) {
      if (is.null(x) || !length(x) || !is.finite(x[1])) return("—")
      format(round(x[1], digits), big.mark = ".", decimal.mark = ",", trim = TRUE)
    }

    output$status_hero_ui <- renderUI({
      res <- current_result()
      status <- classify_stock_status(res$last.B_Bmsy, res$last.F_Fmsy)
      color <- status_color(status)
      confidence <- classify_status_confidence(
        res$last.B_Bmsy, res$lcl.last.B_Bmsy, res$ucl.last.B_Bmsy,
        res$last.F_Fmsy, res$lcl.last.F_Fmsy, res$ucl.last.F_Fmsy)
      div(class = "status-hero", style = paste0("--status-color:", color, ";"),
        div(class = "status-hero-main",
          div(class = "status-hero-icon", icon(status_icon(status))),
          div(
            div(class = "status-hero-name", res$stock),
            div(class = "status-meta", paste("Model", res$btype %||% "None"), "|", paste(res$n_years, "tahun"))
          )),
        div(class = "status-hero-side",
          div(class = "status-hero-badge", status),
          div(class = "confidence-label", paste("Ketegasan:", confidence)))
      )
    })

    output$metrics_row_ui <- renderUI({
      res <- current_result()
      metric <- function(value, label, lower = NULL, upper = NULL, class = "") {
        div(class = paste("metric-card", class),
          div(class = "metric-value", value),
          div(class = "metric-label", label),
          if (!is.null(lower) && !is.null(upper))
            div(class = "metric-sub", paste("95% CI", lower, "-", upper)))
      }
      div(class = "metrics-row",
        metric(safe_number(res$MSY, 1), "MSY (ton/tahun)", safe_number(res$lcl.MSY, 1), safe_number(res$ucl.MSY, 1), "metric-green"),
        metric(safe_number(res$last.B_Bmsy, 2), "B/Bmsy", safe_number(res$lcl.last.B_Bmsy, 2), safe_number(res$ucl.last.B_Bmsy, 2),
               if (res$last.B_Bmsy < .5) "metric-red" else if (res$last.B_Bmsy < .8) "metric-yellow" else "metric-green"),
        metric(safe_number(res$last.F_Fmsy, 2), "F/Fmsy", safe_number(res$lcl.last.F_Fmsy, 2), safe_number(res$ucl.last.F_Fmsy, 2),
               if (res$last.F_Fmsy > 1) "metric-red" else if (res$last.F_Fmsy > .8) "metric-yellow" else "metric-green"),
        metric(safe_number(res$r, 3), "Produktivitas r"),
        metric(safe_number(res$k, 0), "Daya dukung k (ton)")
      )
    })

    make_plot_tag <- function(run_dir, filename) {
      if (is.null(run_dir) || !length(run_dir) || !dir.exists(run_dir)) {
        return(div(class = "empty-plot", icon("folder-open", class = "fa-2x"), "Folder hasil tidak ditemukan."))
      }
      full_path <- file.path(run_dir, filename)
      if (!file.exists(full_path)) {
        return(div(class = "empty-plot", icon("chart-area", class = "fa-2x"), "Grafik ini tidak tersedia."))
      }
      runs_root <- normalizePath("runs", winslash = "/", mustWork = TRUE)
      normalized_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
      prefix <- paste0(runs_root, "/")
      if (!startsWith(normalized_dir, prefix)) {
        return(div(class = "empty-plot", icon("shield", class = "fa-2x"), "Path grafik tidak valid."))
      }
      relative_dir <- substring(normalized_dir, nchar(prefix) + 1)
      version <- as.integer(file.info(full_path)$mtime)
      tags$img(
        src = paste0("runs/", relative_dir, "/", filename, "?v=", version),
        loading = "lazy", decoding = "async", alt = filename,
        class = "result-plot"
      )
    }

    output$active_plot_ui <- renderUI({
      res <- current_result()
      choice <- input$plot_choice %||% "AN"
      stock_safe <- gsub("[^A-Za-z0-9_]", "_", res$stock)
      make_plot_tag(res$workdir, paste0(stock_safe, "_", choice, ".jpg"))
    })
  })
}
