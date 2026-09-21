# app_batch.R
# CMSY++ Batch Assessment Wizard for KOMNASJISKAN
# Multi-species batch processing with group-level justification

local_library <- normalizePath("R_library", mustWork = FALSE)
dir.create(local_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_library, .libPaths()))

# Dependency preflight (avoid concurrent, non-reproducible installs at app startup)
required.packages <- c("shiny", "shinyjs", "bslib", "DT", "processx", "zip", "readr", "shinyvalidate")
new.packages <- required.packages[!(required.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  stop("Missing application packages: ", paste(new.packages, collapse = ", "),
       ". Install dependencies into R_library before launching the app.")
}

library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(processx)
library(readr)
library(shinyvalidate)

# Register resource paths
shiny::addResourcePath("www_assets", "www")
shiny::addResourcePath("runs", "runs")

# Source all modules
source("R/cmsy_runner.R")
source("R/batch_helpers.R")
source("R/cpue_helpers.R")
source("R/mod_batch_import.R")
source("R/mod_batch_cpue.R")
source("R/mod_batch_validate.R")
source("R/mod_batch_params.R")
source("R/mod_batch_run.R")
source("R/mod_batch_results.R")
source("R/mod_batch_summary.R")
source("R/mod_how_to_use.R")

# ============================================================
# THEME
# ============================================================
batch_theme <- bs_theme(
  version = 5,
  primary = "#0f766e",
  secondary = "#2563eb",
  success = "#15803d",
  warning = "#d97706",
  danger = "#dc2626"
)

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  theme = batch_theme,
  title = div(class = "app-title",
    icon("fish"),
    span("cMSY++ Stock Assessment")
  ),
  id = "batch_nav",
  fillable = FALSE,

  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www_assets/batch.css"),
      tags$script(src = "www_assets/custom.js")
    ),

    # Batch wizard step tracker
    div(class = "batch-wizard-bar",
      div(class = "bw-step active", id = "bw_step_1", icon("file-upload"), "1. Import"),
      div(class = "bw-connector"),
      div(class = "bw-step", id = "bw_step_2", icon("water"), "2. Indeks"),
      div(class = "bw-connector"),
      div(class = "bw-step", id = "bw_step_3", icon("shield-alt"), "3. Validasi"),
      div(class = "bw-connector"),
      div(class = "bw-step", id = "bw_step_4", icon("sliders"), "4. Parameter"),
      div(class = "bw-connector"),
      div(class = "bw-step", id = "bw_step_5", icon("play"), "5. Proses"),
      div(class = "bw-connector"),
      div(class = "bw-step", id = "bw_step_6", icon("chart-line"), "6. Hasil"),
      div(class = "bw-connector"),
      div(class = "bw-step", id = "bw_step_7", icon("layer-group"), "7. Ringkasan")
    )
  ),

  # ============================================================
  # TAB 1: IMPORT
  # ============================================================
  nav_panel(
    title = "1. Import",
    value = "tab_import",
    div(class = "batch-page",
      mod_batch_import_ui("batch_import"),
      div(class = "batch-nav",
        actionButton("btn_batch_next_1", "Lanjut: Indeks kelimpahan", class = "btn-success", icon = icon("arrow-right"))
      )
    )
  ),

  nav_panel(
    title = "2. Indeks Kelimpahan",
    value = "tab_cpue",
    div(class = "batch-page",
      mod_batch_cpue_ui("batch_cpue"),
      div(class = "batch-nav",
        actionButton("btn_batch_prev_2", "Kembali", class = "btn-nav-prev", icon = icon("arrow-left")),
        actionButton("btn_batch_next_2", "Lanjut: Validasi", class = "btn-success", icon = icon("arrow-right"))
      )
    )
  ),

  # ============================================================
  # TAB 2: VALIDATE
  # ============================================================
  nav_panel(
    title = "3. Validasi",
    value = "tab_validate",
    div(class = "batch-page",
      mod_batch_validate_ui("batch_validate"),
      div(class = "batch-nav",
        actionButton("btn_batch_prev_3", "Kembali", class = "btn-nav-prev", icon = icon("arrow-left")),
        actionButton("btn_batch_next_3", "Lanjut: Parameter", class = "btn-success", icon = icon("arrow-right"))
      )
    )
  ),

  # ============================================================
  # TAB 3: PARAMETERS
  # ============================================================
  nav_panel(
    title = "4. Parameter",
    value = "tab_params",
    div(class = "batch-page",
      mod_batch_params_ui("batch_params"),
      div(class = "batch-nav",
        actionButton("btn_batch_prev_4", "Kembali", class = "btn-nav-prev", icon = icon("arrow-left")),
        actionButton("btn_batch_next_4", "Lanjut: Proses", class = "btn-success", icon = icon("arrow-right"))
      )
    )
  ),

  # ============================================================
  # TAB 4: RUN
  # ============================================================
  nav_panel(
    title = "5. Proses",
    value = "tab_run",
    div(class = "batch-page",
      mod_batch_run_ui("batch_run"),
      div(class = "batch-nav",
        actionButton("btn_batch_prev_5", "Kembali", class = "btn-nav-prev", icon = icon("arrow-left")),
        actionButton("btn_batch_next_5", "Lanjut: Hasil", class = "btn-success", icon = icon("arrow-right"))
      )
    )
  ),

  # ============================================================
  # TAB 5: RESULTS
  # ============================================================
  nav_panel(
    title = "6. Hasil",
    value = "tab_results",
    div(class = "batch-page",
      mod_batch_results_ui("batch_results"),
      div(class = "batch-nav",
        actionButton("btn_batch_prev_6", "Kembali", class = "btn-nav-prev", icon = icon("arrow-left")),
        actionButton("btn_batch_next_6", "Lanjut: Ringkasan", class = "btn-success", icon = icon("arrow-right"))
      )
    )
  ),

  # ============================================================
  # TAB 6: SUMMARY
  # ============================================================
  nav_panel(
    title = "7. Ringkasan",
    value = "tab_summary",
    div(class = "batch-page",
      mod_batch_summary_ui("batch_summary"),
      div(class = "batch-nav",
        actionButton("btn_batch_prev_7", "Kembali", class = "btn-nav-prev", icon = icon("arrow-left"))
      )
    )
  ),

  nav_panel(
    title = "How to Use",
    value = "tab_help",
    div(class = "batch-page", mod_how_to_use_ui("how_to_use"))
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # --- Module 1: Import ---
  import_res <- mod_batch_import_server("batch_import")

  # --- Module 2: Abundance / CPUE ---
  cpue_res <- mod_batch_cpue_server(
    "batch_cpue",
    catch_df = import_res$catch_df,
    id_df = import_res$id_df
  )

  # --- Module 3: Validate ---
  validate_res <- mod_batch_validate_server(
    "batch_validate",
    catch_df = cpue_res$catch_df,
    id_df = cpue_res$id_df
  )

  # --- Module 4: Parameters ---
  params_res <- mod_batch_params_server(
    "batch_params",
    species_list = import_res$species_list,
    cpue_cv = cpue_res$recommended_cv
  )

  # --- Module 4: Run ---
  run_res <- mod_batch_run_server(
    "batch_run",
    catch_df = cpue_res$catch_df,
    id_df = cpue_res$id_df,
    species_list = import_res$species_list,
    groups = import_res$groups,
    params = params_res
  )

  # --- Module 5: Results ---
  mod_batch_results_server(
    "batch_results",
    batch_results = run_res$results,
    id_df = cpue_res$id_df
  )

  # --- Module 6: Summary ---
  mod_batch_summary_server(
    "batch_summary",
    batch_results = run_res$results,
    id_df = cpue_res$id_df,
    groups = import_res$groups
  )

  # ============================================================
  # WIZARD NAVIGATION
  # ============================================================
  batch_tabs <- c("tab_import", "tab_cpue", "tab_validate", "tab_params", "tab_run", "tab_results", "tab_summary")

  # Update step indicators
  observe({
    req(input$batch_nav)
    curr <- input$batch_nav
    idx <- match(curr, batch_tabs)

    for (i in seq_along(batch_tabs)) {
      step_id <- paste0("bw_step_", i)
      if (!is.na(idx) && i == idx) {
        shinyjs::addClass(step_id, "active")
        shinyjs::removeClass(step_id, "completed")
      } else if (!is.na(idx) && i < idx) {
        shinyjs::removeClass(step_id, "active")
        shinyjs::addClass(step_id, "completed")
      } else {
        shinyjs::removeClass(step_id, "active")
        shinyjs::removeClass(step_id, "completed")
      }
    }
  })

  # Next buttons
  observeEvent(input$btn_batch_next_1, { updateNavbarPage(session, "batch_nav", selected = "tab_cpue") })
  observeEvent(input$btn_batch_next_2, { updateNavbarPage(session, "batch_nav", selected = "tab_validate") })
  observeEvent(input$btn_batch_next_3, { updateNavbarPage(session, "batch_nav", selected = "tab_params") })
  observeEvent(input$btn_batch_next_4, { updateNavbarPage(session, "batch_nav", selected = "tab_run") })
  observeEvent(input$btn_batch_next_5, { updateNavbarPage(session, "batch_nav", selected = "tab_results") })
  observeEvent(input$btn_batch_next_6, { updateNavbarPage(session, "batch_nav", selected = "tab_summary") })

  # Prev buttons
  observeEvent(input$btn_batch_prev_2, { updateNavbarPage(session, "batch_nav", selected = "tab_import") })
  observeEvent(input$btn_batch_prev_3, { updateNavbarPage(session, "batch_nav", selected = "tab_cpue") })
  observeEvent(input$btn_batch_prev_4, { updateNavbarPage(session, "batch_nav", selected = "tab_validate") })
  observeEvent(input$btn_batch_prev_5, { updateNavbarPage(session, "batch_nav", selected = "tab_params") })
  observeEvent(input$btn_batch_prev_6, { updateNavbarPage(session, "batch_nav", selected = "tab_run") })
  observeEvent(input$btn_batch_prev_7, { updateNavbarPage(session, "batch_nav", selected = "tab_results") })
}

# ============================================================
# RUN
# ============================================================
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)
