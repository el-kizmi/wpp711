# Module: select and prepare abundance information before validation and modelling.

mod_batch_cpue_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "batch-cpue",
      div(class = "section-intro",
        h2("Indeks kelimpahan"),
        p("Pilih sumber informasi kelimpahan yang akan dipasangkan dengan tangkapan tahunan.")
      ),
      div(class = "abundance-mode-grid",
        radioButtons(ns("mode"), NULL, selected = "catch_only", inline = FALSE, width = "100%",
          choices = c(
            "Catch-only CMSY" = "catch_only",
            "Standarisasi CPUE dari data operasi" = "internal_cpue",
            "CPUE standar dari pengguna" = "external_cpue",
            "Tangkapan + biomassa absolut" = "biomass"
          ))
      ),
      uiOutput(ns("mode_panel")),
      uiOutput(ns("status_ui")),
      uiOutput(ns("results_ui"))
    )
  )
}

mod_batch_cpue_server <- function(id, catch_df, id_df) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(raw = NULL, external = NULL, fit = NULL, error = NULL)

    output$download_raw_template <- downloadHandler(
      filename = function() "cpue_raw_template.csv",
      content = function(file) file.copy("templates/cpue_raw_template.csv", file, overwrite = TRUE)
    )

    observeEvent(input$mode, {
      rv$error <- NULL
      if (!identical(input$mode, "internal_cpue")) rv$fit <- NULL
      rv$external <- NULL
    }, ignoreInit = TRUE)

    output$mode_panel <- renderUI({
      req(input$mode)
      if (input$mode == "catch_only") {
        return(div(class = "mode-panel compact-panel",
          h3("Catch-only CMSY"),
          p("Sistem menggunakan tangkapan tahunan dan prior stok. Kolom bt tidak digunakan.")))
      }
      if (input$mode == "internal_cpue") {
        stocks <- if (!is.null(id_df())) unique(as.character(id_df()$Stock)) else character(0)
        return(div(class = "mode-panel",
          h3("Standarisasi CPUE"),
          p("Tidak semua kolom template harus terisi. Empat kolom operasi di bawah wajib terisi pada setiap baris yang dianalisis."),
          div(class = "cpue-field-guide",
            div(h4("Wajib per baris"),
                tags$code("nama_alat_tangkap, jumlah_hari_operasi, gt_kapal, berat_ikan_kg"),
                p("Hari operasi dan GT harus lebih dari nol. Berat ikan boleh nol untuk merekam operasi tanpa tangkapan.")),
            div(h4("Wajib untuk analisis"),
                tags$code("Stock, tahun"),
                p("Boleh berasal dari kolom CSV atau satu nilai pengganti pada interface. Nilai pengganti hanya tepat bila seluruh file mewakili stok atau tahun yang sama.")),
            div(h4("Opsional"),
                tags$code("musim, wilayah, dan kovariat lain"),
                p("Boleh kosong jika tidak dipilih. Jika dipilih sebagai kovariat, baris yang nilainya kosong tidak masuk pemodelan dan jumlahnya dilaporkan."))
          ),
          p(class = "field-caution", "Sel kosong tidak pernah dianggap nol. Isi nol hanya untuk tangkapan yang benar-benar nihil."),
          div(class = "cpue-control-grid",
            fileInput(session$ns("raw_file"), "Data operasi CPUE (.csv)", accept = ".csv"),
            selectInput(session$ns("default_stock"), "Stok jika kolom Stock tidak tersedia", choices = stocks),
            numericInput(session$ns("default_year"), "Tahun jika kolom tahun tidak tersedia", value = 2025, min = 1950, max = 2030),
            selectizeInput(session$ns("optional_covariates"), "Kovariat tambahan (opsional)", choices = character(0), multiple = TRUE)
          ),
          div(class = "button-row",
            actionButton(session$ns("calculate"), "Hitung CPUE standar", icon = icon("calculator"), class = "btn-primary"),
            downloadButton(session$ns("download_raw_template"), "Unduh template CPUE", class = "btn-outline")
          )))
      }
      label <- if (input$mode == "biomass") "Data biomassa absolut (.csv)" else "Data CPUE standar (.csv)"
      div(class = "mode-panel",
        h3(if (input$mode == "biomass") "Tangkapan + biomassa absolut" else "CPUE standar pengguna"),
        p("Format: Stock, yr, bt. Jika tidak mengunggah file terpisah, sistem menggunakan kolom bt dari Catch CSV."),
        fileInput(session$ns("index_file"), label, accept = ".csv"))
    })

    observeEvent(input$raw_file, {
      req(input$raw_file)
      rv$error <- NULL
      rv$fit <- NULL
      rv$raw <- tryCatch(
        read.csv(input$raw_file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) { rv$error <- conditionMessage(e); NULL }
      )
      if (!is.null(rv$raw)) {
        canonical <- tryCatch(canonicalize_cpue_data(rv$raw, input$default_stock, input$default_year), error = function(e) NULL)
        choices <- if (is.null(canonical)) character(0) else canonical$optional
        updateSelectizeInput(session, "optional_covariates", choices = choices, selected = character(0), server = TRUE)
      }
    })

    observeEvent(input$index_file, {
      req(input$index_file)
      rv$error <- NULL
      rv$external <- tryCatch(read_abundance_index(input$index_file$datapath), error = function(e) {
        rv$error <- conditionMessage(e)
        NULL
      })
    })

    observeEvent(input$calculate, {
      req(rv$raw)
      rv$error <- NULL
      rv$fit <- tryCatch(
        standardize_cpue_dataset(rv$raw, input$default_stock, input$default_year,
                                 input$optional_covariates %||% character(0)),
        error = function(e) { rv$error <- conditionMessage(e); NULL }
      )
    })

    active_index <- reactive({
      req(input$mode)
      if (input$mode == "internal_cpue") {
        if (is.null(rv$fit)) return(NULL)
        return(rv$fit$index)
      }
      if (input$mode %in% c("external_cpue", "biomass")) {
        if (!is.null(rv$external)) return(rv$external)
        cdf <- catch_df()
        if (!is.null(cdf) && all(c("Stock", "yr", "bt") %in% names(cdf))) {
          out <- cdf[, c("Stock", "yr", "bt"), drop = FALSE]
          out$bt <- suppressWarnings(as.numeric(out$bt))
          return(out[is.finite(out$bt) & out$bt > 0, , drop = FALSE])
        }
      }
      NULL
    })

    output$status_ui <- renderUI({
      req(input$mode)
      if (!is.null(rv$error)) return(div(class = "inline-alert alert-error", icon("circle-xmark"), rv$error))
      if (input$mode == "catch_only") return(div(class = "inline-alert alert-info", "Mode aktif: catch-only CMSY."))
      idx <- active_index()
      if (is.null(idx) || !nrow(idx)) return(div(class = "inline-alert alert-muted", "Belum ada indeks yang dapat diteruskan."))
      counts <- table(idx$Stock)
      ready <- sum(counts >= 3)
      div(class = if (ready > 0) "inline-alert alert-success" else "inline-alert alert-warning",
          paste(nrow(idx), "nilai indeks untuk", length(counts), "stok.", ready, "stok memiliki sedikitnya 3 tahun."))
    })

    output$results_ui <- renderUI({
      req(input$mode == "internal_cpue", !is.null(rv$fit))
      tagList(
        div(class = "cpue-result-grid",
          div(class = "summary-section",
            div(class = "section-header", "Indeks tahunan"),
            plotOutput(session$ns("index_plot"), height = "320px")
          ),
          div(class = "summary-section",
            div(class = "section-header", "Diagnostik model"),
            DTOutput(session$ns("diagnostic_table"))
          )
        ),
        if (length(rv$fit$warnings)) div(class = "inline-alert alert-warning", paste(rv$fit$warnings, collapse = " ")),
        if (length(rv$fit$errors)) div(class = "inline-alert alert-error", paste(rv$fit$errors, collapse = " ")),
        div(class = "summary-section", DTOutput(session$ns("index_table")))
      )
    })

    output$index_plot <- renderPlot({
      req(rv$fit)
      d <- rv$fit$index
      stocks <- unique(d$Stock)
      cols <- grDevices::hcl.colors(length(stocks), "Dark 3")
      ylim <- range(c(d$lcl, d$ucl), finite = TRUE)
      plot(range(d$yr), ylim, type = "n", xlab = "Tahun", ylab = "Indeks CPUE standar",
           las = 1, bty = "l", col.axis = "#334155", col.lab = "#0f172a")
      abline(h = 1, col = "#94a3b8", lty = 3)
      for (i in seq_along(stocks)) {
        z <- d[d$Stock == stocks[i], , drop = FALSE]
        polygon(c(z$yr, rev(z$yr)), c(z$lcl, rev(z$ucl)), border = NA,
                col = adjustcolor(cols[i], alpha.f = 0.14))
        lines(z$yr, z$bt, col = cols[i], lwd = 2)
        points(z$yr, z$bt, col = cols[i], pch = 19)
      }
      if (length(stocks) <= 8) legend("topright", stocks, col = cols, lwd = 2, bty = "n", cex = 0.8)
    }, res = 110)

    output$diagnostic_table <- renderDT({
      req(rv$fit)
      datatable(rv$fit$diagnostics, rownames = FALSE,
                options = list(dom = "t", scrollX = TRUE, pageLength = 10), class = "table-clean")
    })
    output$index_table <- renderDT({
      req(rv$fit)
      datatable(rv$fit$index, rownames = FALSE,
                options = list(pageLength = 12, scrollX = TRUE, dom = "tip"), class = "table-clean")
    })

    analysis_catch <- reactive({
      cdf <- catch_df()
      req(cdf, input$mode)
      out <- cdf
      if (!"bt" %in% names(out)) out$bt <- NA_real_
      if (input$mode == "catch_only") {
        out$bt <- NA_real_
      } else {
        idx <- active_index()
        if (!is.null(idx) && nrow(idx)) out <- merge_abundance_index(out, idx)
      }
      out
    })

    analysis_id <- reactive({
      id <- id_df()
      req(id, input$mode)
      if (!"btype" %in% names(id)) id$btype <- "None"
      if (!"e.creep" %in% names(id)) id$e.creep <- NA_real_
      id$btype <- "None"
      if (input$mode != "catch_only") {
        idx <- active_index()
        if (!is.null(idx) && nrow(idx)) {
          sufficient <- names(which(table(idx$Stock) >= 3))
          selected <- id$Stock %in% sufficient
          id$btype[selected] <- if (input$mode == "biomass") "biomass" else "CPUE"
          id$e.creep[selected] <- NA_real_
        }
      }
      id
    })

    recommended_cv <- reactive({
      if (input$mode != "internal_cpue" || is.null(rv$fit)) return(NULL)
      value <- median(rv$fit$diagnostics$recommended_cv, na.rm = TRUE)
      if (!is.finite(value)) NULL else max(0.05, min(1, value))
    })

    list(
      mode = reactive(input$mode),
      catch_df = analysis_catch,
      id_df = analysis_id,
      index = active_index,
      recommended_cv = recommended_cv,
      ready = reactive({
        if (input$mode == "catch_only") return(TRUE)
        idx <- active_index()
        !is.null(idx) && nrow(idx) > 0 && any(table(idx$Stock) >= 3)
      })
    )
  })
}
