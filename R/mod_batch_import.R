# R/mod_batch_import.R
# Module: Batch data import with auto-group detection

mod_batch_import_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "batch-import",

      # Upload Area
      div(class = "upload-grid",
        div(class = "upload-zone",
          div(class = "upload-zone-icon", icon("file-csv")),
          div(class = "upload-zone-label", "Catch Data CSV"),
          div(class = "upload-zone-hint", "Kolom: Stock, yr, ct (kg); bt opsional"),
          div(class = "file-input-hidden",
            fileInput(ns("catch_file"), NULL, accept = c(".csv"))
          ),
          div(class = "upload-zone-btn", "Pilih File")
        ),
        div(class = "upload-zone",
          div(class = "upload-zone-icon", icon("id-card")),
          div(class = "upload-zone-label", "Stock ID / Metadata CSV"),
          div(class = "upload-zone-hint", "Kolom: Stock, Group, + metadata lainnya"),
          div(class = "file-input-hidden",
            fileInput(ns("id_file"), NULL, accept = c(".csv"))
          ),
          div(class = "upload-zone-btn", "Pilih File")
        )
      ),

      # Template download
      div(class = "template-row",
        span(class = "template-label", icon("download"), "Download template kosong:"),
        downloadButton(ns("dl_catch_tpl"), "Catch Template", class = "btn-sm btn-outline", icon = icon("file-csv")),
        downloadButton(ns("dl_id_tpl"), "ID Template", class = "btn-sm btn-outline", icon = icon("file-csv"))
      ),

      # Group Detection Summary
      uiOutput(ns("group_summary_ui")),

      # Stock Preview Table
      uiOutput(ns("stock_preview_ui"))
    )
  )
}

mod_batch_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      catch_df = NULL,
      id_df = NULL,
      catch_path = NULL,
      id_path = NULL,
      groups = NULL,
      species_list = NULL
    )

    # Download templates
    output$dl_catch_tpl <- downloadHandler(
      filename = function() "batch_catch_template.csv",
      content = function(file) file.copy("templates/batch_catch_template.csv", file)
    )

    output$dl_id_tpl <- downloadHandler(
      filename = function() "batch_id_template.csv",
      content = function(file) file.copy("templates/batch_id_template.csv", file)
    )

    # Observe catch file upload
    observeEvent(input$catch_file, {
      req(input$catch_file)
      df <- tryCatch(
        read.csv(input$catch_file$datapath, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df) && all(c("Stock", "yr", "ct") %in% names(df))) {
        rv$catch_df <- df
        rv$catch_path <- input$catch_file$datapath
        showNotification("Catch data loaded.", type = "message")
      } else {
        showNotification("CSV harus memiliki kolom Stock, yr, dan ct", type = "error")
      }
    })

    # Observe ID file upload
    observeEvent(input$id_file, {
      req(input$id_file)
      df <- tryCatch(
        read.csv(input$id_file$datapath, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df) && all(c("Stock", "Group") %in% names(df))) {
        rv$id_df <- df
        rv$id_path <- input$id_file$datapath
        showNotification("Stock ID data loaded.", type = "message")
      } else {
        showNotification("CSV harus punya kolom: Stock, Group", type = "error")
      }
    })

    # Group detection
    observe({
      req(rv$id_df)
      df <- rv$id_df
      if ("Group" %in% names(df)) {
        groups <- sort(unique(df$Group))
        rv$groups <- groups
        rv$species_list <- split(df$Stock, df$Group)
      }
    })

    # Group summary
    output$group_summary_ui <- renderUI({
      req(rv$groups, rv$id_df, rv$catch_df)

      group_cards <- lapply(rv$groups, function(g) {
        species_in_group <- rv$id_df$Stock[rv$id_df$Group == g]
        n <- length(species_in_group)
        n_with_catch <- sum(species_in_group %in% unique(rv$catch_df$Stock))

        div(class = "group-chip",
          div(class = "group-chip-icon",
            if (grepl("[Ll]arge", g)) icon("fish") else icon("shrimp")
          ),
          div(class = "group-chip-info",
            div(class = "group-chip-name", g),
            div(class = "group-chip-count",
              n_with_catch, "/", n, " spesies dengan data catch"
            )
          )
        )
      })

      div(class = "group-summary-panel",
        div(class = "group-summary-title", icon("layer-group"), "Kelompok Terdeteksi"),
        div(class = "group-chips", do.call(tagList, group_cards))
      )
    })

    # Stock preview table
    output$stock_preview_ui <- renderUI({
      req(rv$catch_df, rv$id_df)

      catch_stocks <- unique(rv$catch_df$Stock)
      id_stocks <- unique(rv$id_df$Stock)
      missing_in_id <- setdiff(catch_stocks, id_stocks)
      missing_in_catch <- setdiff(id_stocks, catch_stocks)

      # Build preview dataframe
      all_stocks <- unique(c(catch_stocks, id_stocks))
      preview_df <- data.frame(
        Stock = all_stocks,
        Group = sapply(all_stocks, function(s) {
          grp <- rv$id_df$Group[rv$id_df$Stock == s]
          if (length(grp) > 0) grp[1] else NA
        }),
        Catch_Years = sapply(all_stocks, function(s) {
          n <- sum(rv$catch_df$Stock == s)
          if (n > 0) n else 0
        }),
        Has_ID = sapply(all_stocks, function(s) s %in% id_stocks),
        Status = sapply(all_stocks, function(s) {
          if (s %in% missing_in_id) return("Missing ID")
          if (s %in% missing_in_catch) return("Missing Catch")
          return("Ready")
        }),
        stringsAsFactors = FALSE
      )
      preview_df <- preview_df[order(preview_df$Group, preview_df$Stock), ]

      output$preview_dt <- renderDT({
        datatable(
          preview_df,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = "ftip",
            ordering = TRUE
          ),
          class = "table-clean",
          rownames = FALSE
        )
      })

      div(class = "stock-preview-panel",
        div(class = "panel-title",
          icon("table"), "Daftar Spesies",
          span(class = "panel-badge", nrow(preview_df), "spesies")
        ),
        DTOutput(ns("preview_dt"))
      )
    })

    return(list(
      catch_df = reactive(rv$catch_df),
      id_df = reactive(rv$id_df),
      catch_path = reactive(rv$catch_path),
      id_path = reactive(rv$id_path),
      groups = reactive(rv$groups),
      species_list = reactive(rv$species_list)
    ))
  })
}
