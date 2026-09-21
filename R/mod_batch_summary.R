# R/mod_batch_summary.R
# Module: Group summary, comparison, and justification

mod_batch_summary_ui <- function(id) {
  ns <- NS(id)
  div(class = "batch-summary",
    div(id = ns("empty_state"), class = "empty-state",
      icon("inbox", class = "fa-3x"), h3("Belum ada hasil"),
      p("Jalankan model sebelum membuka ringkasan.")),
    div(id = ns("summary_content"), style = "display:none;",
      uiOutput(ns("group_cards_ui")),
      div(class = "summary-section",
        div(class = "section-header", icon("table"), "Status setiap stok"),
        DTOutput(ns("matrix_dt"))
      ),
      div(class = "summary-section",
        div(class = "summary-chart-header",
          div(class = "section-header", icon("chart-bar"), "Distribusi hasil per kelompok"),
          selectInput(ns("metric_choice"), NULL, width = "240px",
            choices = c("MSY (ton/tahun)" = "MSY", "B/Bmsy" = "last.B_Bmsy", "F/Fmsy" = "last.F_Fmsy"))
        ),
        div(class = "summary-chart-frame", plotOutput(ns("comparison_plot"), height = "380px"))
      ),
      uiOutput(ns("narrative_ui")),
      div(class = "justification-box",
        div(class = "justification-header", icon("info-circle"), "Cara membaca ringkasan"),
        div(class = "justification-body",
          tags$ul(
            tags$li(tags$strong("Status stok: "), "ditentukan dari B/Bmsy dan F/Fmsy. Selalu baca bersama interval 95%."),
            tags$li(tags$strong("Statistik kelompok: "), "median dan rentang menggambarkan variasi antarstok, bukan satu stok gabungan."),
            tags$li(tags$strong("Total MSY: "), "jumlah MSY individual dan bukan MSY ekosistem atau rekomendasi TAC."),
            tags$li(tags$strong("Batas model: "), "interaksi antarspesies dan perubahan ekosistem tidak dimodelkan.")
          )
        )
      ),
      div(class = "summary-export",
        downloadButton(ns("download_summary_csv"), "Unduh ringkasan kelompok", class = "btn-primary"),
        downloadButton(ns("download_species_csv"), "Unduh hasil seluruh stok", class = "btn-outline")
      )
    )
  )
}

mod_batch_summary_server <- function(id, batch_results, id_df, groups) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    has_results <- reactive({
      !is.null(batch_results()) && length(batch_results()) > 0
    })

    observe({
      if (has_results()) {
        shinyjs::hide("empty_state")
        shinyjs::show("summary_content")
      } else {
        shinyjs::show("empty_state")
        shinyjs::hide("summary_content")
      }
    })

    # Enrich results with group info
    enriched_results <- reactive({
      req(has_results(), id_df())
      results <- batch_results()
      id <- id_df()
      for (stk in names(results)) {
        grp <- id$Group[id$Stock == stk]
        results[[stk]]$group <- if (length(grp) > 0) grp[1] else "Unknown"
      }
      results
    })

    # Compute group stats
    group_stats <- reactive({
      req(enriched_results(), groups())
      stats_list <- list()
      for (g in groups()) {
        stats_list[[g]] <- compute_group_stats(enriched_results(), g)
      }
      stats_list
    })

    # Status matrix data (reactive)
    matrix_data <- reactive({
      req(has_results(), id_df())
      res <- enriched_results()
      all_stocks <- names(res)
      if (length(all_stocks) == 0) return(NULL)

      data.frame(
        Stock = all_stocks,
        Group = sapply(all_stocks, function(s) res[[s]]$group %||% "Unknown"),
        MSY = sapply(all_stocks, function(s) round(res[[s]]$MSY, 2)),
        B_Bmsy = sapply(all_stocks, function(s) round(res[[s]]$last.B_Bmsy, 3)),
        F_Fmsy = sapply(all_stocks, function(s) round(res[[s]]$last.F_Fmsy, 3)),
        Status = sapply(all_stocks, function(s) classify_stock_status(res[[s]]$last.B_Bmsy, res[[s]]$last.F_Fmsy)),
        stringsAsFactors = FALSE
      )
    })

    # Group cards
    output$group_cards_ui <- renderUI({
      req(has_results())
      gs <- tryCatch(group_stats(), error = function(e) NULL)
      req(gs)

      cards <- lapply(names(gs), function(g) {
        s <- gs[[g]]
        if (is.null(s)) return(NULL)

        div(class = "group-summary-card",
          div(class = "gsc-header",
            if (grepl("[Ll]arge", g)) icon("fish", class = "gsc-icon") else icon("shrimp", class = "gsc-icon"),
            div(class = "gsc-title", g),
            div(class = "gsc-subtitle", s$n_species, " spesies")
          ),
          div(class = "gsc-stats",
            div(class = "gsc-stat",
              div(class = "gsc-stat-value gsc-green", format(round(s$msy$total, 0), big.mark = ",")),
              div(class = "gsc-stat-label", "Total MSY (ton)")
            ),
            div(class = "gsc-stat",
              div(class = "gsc-stat-value", round(s$bbmsy$median, 2)),
              div(class = "gsc-stat-label", "Median B/Bmsy")
            ),
            div(class = "gsc-stat",
              div(class = "gsc-stat-value", round(s$ffmsy$median, 2)),
              div(class = "gsc-stat-label", "Median F/Fmsy")
            )
          ),
          div(class = "gsc-status-bar",
            div(class = "status-bar-segment", style = paste0("width: ", s$status$healthy_pct, "%; background: #10b981;"), title = paste0("Healthy: ", round(s$status$healthy_pct, 1), "%")),
            div(class = "status-bar-segment", style = paste0("width: ", s$status$recovering_pct, "%; background: #f59e0b;"), title = paste0("Recovering: ", round(s$status$recovering_pct, 1), "%")),
            div(class = "status-bar-segment", style = paste0("width: ", s$status$overfishing_pct, "%; background: #f97316;"), title = paste0("Overfishing: ", round(s$status$overfishing_pct, 1), "%")),
            div(class = "status-bar-segment", style = paste0("width: ", s$status$depleted_pct, "%; background: #ef4444;"), title = paste0("Depleted: ", round(s$status$depleted_pct, 1), "%"))
          ),
          div(class = "gsc-status-legend",
            span(class = "legend-dot", style = "background: #10b981;", "Healthy"),
            span(class = "legend-dot", style = "background: #f59e0b;", "Recovering"),
            span(class = "legend-dot", style = "background: #f97316;", "Overfishing"),
            span(class = "legend-dot", style = "background: #ef4444;", "Depleted")
          )
        )
      })

      do.call(tagList, Filter(Negate(is.null), cards))
    })

    # Status matrix DT (at top level, not nested in renderUI)
    output$matrix_dt <- renderDT({
      tbl <- matrix_data()
      req(tbl)

      dt <- datatable(tbl, options = list(
        pageLength = 50, scrollX = TRUE, dom = "ft",
        order = list(list(5, "asc"))
      ), class = "table-clean", rownames = FALSE)

      dt <- dt %>% formatStyle("Status",
        backgroundColor = styleEqual(
          c("Healthy", "Recovering", "Overfishing", "Depleted", "Underfished"),
          c("#10b98120", "#f59e0b20", "#f9731620", "#ef444420", "#3b82f620")
        ),
        color = styleEqual(
          c("Healthy", "Recovering", "Overfishing", "Depleted", "Underfished"),
          c("#10b981", "#f59e0b", "#f97316", "#ef4444", "#3b82f6")
        ),
        fontWeight = "bold"
      )
      dt
    })

    # Render one comparison at a time to avoid three simultaneous devices.
    output$comparison_plot <- renderPlot({
      req(has_results())
      res <- tryCatch(enriched_results(), error = function(e) NULL)
      req(res, groups(), input$metric_choice)
      labels <- c(MSY = "MSY (ton/tahun)", last.B_Bmsy = "B/Bmsy", last.F_Fmsy = "F/Fmsy")
      plot_group_boxplot(res, input$metric_choice, labels[[input$metric_choice]], groups())
    }, res = 110)

    # Narrative
    output$narrative_ui <- renderUI({
      req(has_results())
      gs <- tryCatch(group_stats(), error = function(e) NULL)
      req(gs)

      narratives <- lapply(names(gs), function(g) {
        narr <- generate_group_narrative(gs[[g]])
        div(class = "narrative-card",
          div(class = "narrative-title", g),
          div(class = "narrative-text", narr)
        )
      })

      div(class = "summary-section",
        div(class = "section-header", icon("file-alt"), "Ringkasan Naratif"),
        do.call(tagList, narratives)
      )
    })

    # Download handlers
    output$download_summary_csv <- downloadHandler(
      filename = function() paste0("batch_summary_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        gs <- group_stats()
        summary_df <- build_group_comparison_table(gs)
        write.csv(summary_df, file, row.names = FALSE)
      }
    )

    output$download_species_csv <- downloadHandler(
      filename = function() paste0("batch_species_", format(Sys.time(), "%Y%m%d"), ".csv"),
      content = function(file) {
        res <- enriched_results()
        species_df <- build_species_table(res)
        write.csv(species_df, file, row.names = FALSE)
      }
    )
  })
}

# ============================================================
# Boxplot helper
# ============================================================
plot_group_boxplot <- function(results_list, field, ylab, group_names) {
  if (is.null(results_list) || length(results_list) == 0 || is.null(group_names) || length(group_names) == 0) {
    plot.new()
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "transparent")
    text(0.5, 0.5, "Belum ada data", cex = 1.5, col = "#94a3b8", font = 2)
    return(invisible(NULL))
  }

  all_data <- data.frame()
  for (g in group_names) {
    group_res <- Filter(function(x) !is.null(x) && !is.null(x$group) && x$group == g, results_list)
    if (length(group_res) == 0) next
    vals <- sapply(group_res, function(x) x[[field]])
    if (is.null(vals) || length(vals) == 0) next
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) next
    all_data <- rbind(all_data, data.frame(Group = g, Value = vals, stringsAsFactors = FALSE))
  }

  if (nrow(all_data) == 0) {
    plot.new()
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "transparent")
    text(0.5, 0.5, "Belum ada data", cex = 1.5, col = "#94a3b8", font = 2)
    return(invisible(NULL))
  }

  # Custom colors
  base_colors <- c("#10b981", "#3b82f6", "#f59e0b", "#ef4444", "#8b5cf6", "#06b6d4", "#ec4899", "#84cc16")
  grp_colors <- rep(base_colors, length.out = length(group_names))

  par(bg = "transparent", col.axis = "#334155", col.lab = "#0f172a",
      mar = c(7, 4.5, 1, 1), las = 2)
  boxplot(Value ~ Group, data = all_data,
          col = grp_colors[1:length(group_names)],
          border = "#475569",
          main = NULL,
          ylab = ylab,
          xlab = NULL,
          cex.axis = 0.85,
          cex.lab = 1,
          outcol = "#94a3b8",
          outpch = 19,
          staplecol = "#475569")

  # Add jittered points
  present_groups <- unique(all_data$Group)
  for (i in seq_along(present_groups)) {
    grp_vals <- all_data$Value[all_data$Group == present_groups[i]]
    if (length(grp_vals) > 0) {
      set.seed(42)
      jitter_x <- rep(i, length(grp_vals)) + runif(length(grp_vals), -0.15, 0.15)
      points(jitter_x, grp_vals, pch = 19, cex = 0.7, col = adjustcolor(grp_colors[i], alpha.f = 0.6))
    }
  }

  # Reference line for B/Bmsy and F/Fmsy
  if (field == "last.B_Bmsy") abline(h = 1.0, lty = 2, col = "#f59e0b80")
  if (field == "last.F_Fmsy") abline(h = 1.0, lty = 2, col = "#ef444480")
}
