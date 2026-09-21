# R/mod_batch_params.R
# Module: Global and per-species parameter configuration

mod_batch_params_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "batch-params",

      # Global Defaults Section
      div(class = "params-section",
        div(class = "params-section-title", icon("sliders"), "Parameter utama"),

        div(class = "params-grid",
          div(class = "param-card",
            div(class = "param-label", "MCMC Chains"),
            numericInput(ns("global_chains"), NULL, value = 3, min = 2, max = 5)
          ),
          div(class = "param-card",
            div(class = "param-label", "Prior draws (CMSY)"),
            numericInput(ns("global_n"), NULL, value = 10000, min = 1000, max = 50000, step = 1000)
          ),
          div(class = "param-card",
            div(class = "param-label", "CV Catch"),
            numericInput(ns("global_cv_c"), NULL, value = 0.15, min = 0.01, max = 1.0, step = 0.01)
          ),
          div(class = "param-card",
            div(class = "param-label", "CV CPUE"),
            numericInput(ns("global_cv_cpue"), NULL, value = 0.20, min = 0.01, max = 1.0, step = 0.01)
          ),
          div(class = "param-card",
            div(class = "param-label", "SigmaR"),
            numericInput(ns("global_sigma_r"), NULL, value = 0.10, min = 0.01, max = 1.0, step = 0.01)
          ),
          div(class = "param-card",
            div(class = "param-label", "Bandwidth"),
            numericInput(ns("global_bw"), NULL, value = 3, min = 1, max = 10)
          ),
          div(class = "param-card",
            div(class = "param-label", "Minimum tahun indeks"),
            numericInput(ns("global_nab"), NULL, value = 3, min = 3, max = 20)
          ),
          div(class = "param-card",
            div(class = "param-label", "Max Parallel"),
            numericInput(ns("global_max_parallel"), NULL, value = 3, min = 1, max = 8)
          )
        ),

        # Preset buttons
        div(class = "preset-row",
          actionButton(ns("preset_conservative"), "Konservatif", class = "btn-sm btn-preset"),
          actionButton(ns("preset_standard"), "Standar", class = "btn-sm btn-preset btn-preset-active"),
          actionButton(ns("preset_fast"), "Cepat", class = "btn-sm btn-preset")
        ),
        uiOutput(ns("cpue_cv_hint"))
      ),

      # Plot toggles
      div(class = "params-section",
        div(class = "params-section-title", icon("chart-bar"), "Grafik dan keluaran"),

        div(class = "toggle-grid",
          div(class = "toggle-item",
            checkboxInput(ns("global_kobe"), "Kobe Plot", value = TRUE),
            checkboxInput(ns("global_bsm_fits"), "BSM Fits", value = TRUE)
          ),
          div(class = "toggle-item",
            checkboxInput(ns("global_pp"), "Prior vs Posterior", value = TRUE),
            checkboxInput(ns("global_rk_diags"), "r-k Diagnostics", value = TRUE)
          ),
          div(class = "toggle-item",
            checkboxInput(ns("global_retros"), "Retrospective", value = TRUE),
            checkboxInput(ns("global_write_rdata"), "Save RData", value = TRUE)
          ),
          div(class = "toggle-item",
            checkboxInput(ns("global_bt4pr"), "Gunakan indeks untuk prior B/k", value = FALSE)
          )
        )
      ),

      # Per-species override table
      div(class = "params-section",
        div(class = "params-section-title", icon("pencil-alt"), "Override per Spesies"),
        div(class = "params-hint", "Klik sel untuk edit. Kosongkan = gunakan global default."),
        DTOutput(ns("override_table"))
      )
    )
  )
}

mod_batch_params_server <- function(id, species_list, cpue_cv = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      override_data = NULL
    )

    # Preset: Conservative
    observeEvent(input$preset_conservative, {
      updateNumericInput(session, "global_cv_c", value = 0.20)
      updateNumericInput(session, "global_cv_cpue", value = 0.30)
      updateNumericInput(session, "global_n", value = 15000)
      updateNumericInput(session, "global_chains", value = 3)
      showNotification("Conservative preset applied", type = "message")
    })

    output$cpue_cv_hint <- renderUI({
      value <- cpue_cv()
      if (is.null(value) || !is.finite(value)) return(NULL)
      div(class = "parameter-recommendation",
        span("CV hasil standardisasi:", strong(format(round(value, 3), nsmall = 3))),
        actionButton(ns("apply_cpue_cv"), "Gunakan nilai ini", class = "btn-sm btn-outline")
      )
    })

    observeEvent(input$apply_cpue_cv, {
      value <- cpue_cv()
      req(!is.null(value), is.finite(value))
      updateNumericInput(session, "global_cv_cpue", value = round(value, 3))
    })

    # Preset: Standard
    observeEvent(input$preset_standard, {
      updateNumericInput(session, "global_cv_c", value = 0.15)
      updateNumericInput(session, "global_cv_cpue", value = 0.20)
      updateNumericInput(session, "global_n", value = 10000)
      updateNumericInput(session, "global_chains", value = 3)
      showNotification("Standard preset applied", type = "message")
    })

    # Preset: Fast
    observeEvent(input$preset_fast, {
      updateNumericInput(session, "global_cv_c", value = 0.15)
      updateNumericInput(session, "global_cv_cpue", value = 0.20)
      updateNumericInput(session, "global_n", value = 5000)
      updateNumericInput(session, "global_chains", value = 2)
      showNotification("Fast preset applied", type = "message")
    })

    # Override table
    observe({
      req(species_list())
      all_stocks <- unlist(species_list())
      override_df <- data.frame(
        Stock       = all_stocks,
        CV_C        = rep(NA, length(all_stocks)),
        CV_cpue     = rep(NA, length(all_stocks)),
        sigmaR      = rep(NA, length(all_stocks)),
        n_chains    = rep(NA, length(all_stocks)),
        n_iter      = rep(NA, length(all_stocks)),
        e_creep     = rep(NA, length(all_stocks)),
        force_cmsy  = rep(NA, length(all_stocks)),
        stringsAsFactors = FALSE
      )
      rv$override_data <- override_df
    })

    output$override_table <- renderDT({
      req(rv$override_data)
      dt <- datatable(
        rv$override_data,
        editable = TRUE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip",
          columnDefs = list(
            list(className = "dt-center", targets = 1:7)
          )
        ),
        class = "table-clean",
        rownames = FALSE
      )
      dt
    })

    # Handle cell edit
    observeEvent(input$override_table_cell_edit, {
      info <- input$override_table_cell_edit
      rv$override_data <- DT::editData(rv$override_data, info, proxy = ns("override_table"))
    })

    # Return params
    return(list(
      n_chains = reactive(input$global_chains),
      n = reactive(input$global_n),
      CV_C = reactive(input$global_cv_c),
      CV_cpue = reactive(input$global_cv_cpue),
      sigmaR = reactive(input$global_sigma_r),
      bw = reactive(input$global_bw),
      nab = reactive(input$global_nab),
      bt4pr = reactive(input$global_bt4pr),
      max_parallel = reactive(input$global_max_parallel),
      kobe_plot = reactive(input$global_kobe),
      BSMfits_plot = reactive(input$global_bsm_fits),
      pp_plot = reactive(input$global_pp),
      rk_diags = reactive(input$global_rk_diags),
      retros = reactive(input$global_retros),
      write_rdata = reactive(input$global_write_rdata),
      override_data = reactive(rv$override_data)
    ))
  })
}
