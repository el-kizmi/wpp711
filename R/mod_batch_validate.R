# R/mod_batch_validate.R
# Module: Batch validation for all species

mod_batch_validate_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "batch-validate",

      # Action bar
      div(class = "action-bar",
        actionButton(ns("btn_validate_all"), "Jalankan Validasi", class = "btn-primary", icon = icon("shield-alt")),
        div(class = "action-bar-info", uiOutput(ns("summary_badge")))
      ),

      # Filter row
      div(class = "filter-row",
        actionButton(ns("filter_all"), "Semua", class = "btn-filter active"),
        actionButton(ns("filter_error"), "Error", class = "btn-filter btn-filter-error"),
        actionButton(ns("filter_warning"), "Warning", class = "btn-filter btn-filter-warning"),
        actionButton(ns("filter_pass"), "Pass", class = "btn-filter btn-filter-pass")
      ),

      # Validation results
      uiOutput(ns("validation_ui"))
    )
  )
}

mod_batch_validate_server <- function(id, catch_df, id_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      results = NULL,
      summary = NULL,
      filter = "all"
    )

    # Filter buttons
    observeEvent(input$filter_all,   { rv$filter <- "all" })
    observeEvent(input$filter_error, { rv$filter <- "error" })
    observeEvent(input$filter_warning, { rv$filter <- "warning" })
    observeEvent(input$filter_pass,  { rv$filter <- "pass" })

    observeEvent(input$btn_validate_all, {
      req(catch_df(), id_df())
      c_df <- catch_df()
      i_df <- id_df()
      all_stocks <- unique(c(as.character(c_df$Stock), as.character(i_df$Stock)))

      withProgress(message = "Validasi data...", value = 0, {
        all_checks <- list()
        for (i in seq_along(all_stocks)) {
          stk <- all_stocks[i]
          incProgress(1 / length(all_stocks), detail = paste("Memeriksa", stk, "..."))
          stk_checks <- validate_single_stock(c_df, i_df, stk)
          all_checks <- c(all_checks, stk_checks)
        }
      })

      # Summary
      statuses <- sapply(all_checks, function(x) x$Status)
      n_error   <- sum(statuses == "ERROR", na.rm = TRUE)
      n_warning <- sum(statuses == "WARNING", na.rm = TRUE)
      n_pass    <- sum(statuses == "PASS", na.rm = TRUE)
      n_stocks_with_error <- length(unique(sapply(all_checks[statuses == "ERROR"], function(x) x$Stock)))

      rv$summary <- list(
        total = length(all_checks),
        error = n_error,
        warning = n_warning,
        pass = n_pass,
        stocks_total = length(all_stocks),
        stocks_ok = length(all_stocks) - n_stocks_with_error
      )
      rv$results <- all_checks

      if (n_error > 0) {
        showNotification(paste(n_error, "error ditemukan pada", n_stocks_with_error, "spesies"), type = "error")
      } else {
        showNotification("Semua spesies lolos validasi!", type = "message")
      }
    })

    # Summary badge
    output$summary_badge <- renderUI({
      req(rv$summary)
      s <- rv$summary

      div(class = "validation-summary-badges",
        span(class = "badge badge-pass", icon("check-circle"), s$pass, " Pass"),
        span(class = "badge badge-warn", icon("exclamation-triangle"), s$warning, " Warning"),
        span(class = "badge badge-error", icon("times-circle"), s$error, " Error"),
        span(class = "badge badge-info", icon("fish"), s$stocks_ok, "/", s$stocks_total, " spesies OK")
      )
    })

    # Validation UI
    output$validation_ui <- renderUI({
      req(rv$results)

      # Group by stock
      checks <- rv$results
      stock_names <- unique(sapply(checks, function(x) x$Stock))
      f <- rv$filter

      stock_cards <- lapply(stock_names, function(stk) {
        stk_checks <- Filter(function(x) x$Stock == stk, checks)
        statuses <- sapply(stk_checks, function(x) x$Status)

        # Apply filter
        if (f == "error" && !any(statuses == "ERROR")) return(NULL)
        if (f == "warning" && !any(statuses == "WARNING") && !any(statuses == "ERROR")) return(NULL)
        if (f == "pass" && any(statuses == "ERROR")) return(NULL)

        # Determine stock-level status
        if (any(statuses == "ERROR")) {
          stock_class <- "stock-card-error"
          stock_icon <- icon("times-circle")
        } else if (any(statuses == "WARNING")) {
          stock_class <- "stock-card-warning"
          stock_icon <- icon("exclamation-triangle")
        } else {
          stock_class <- "stock-card-pass"
          stock_icon <- icon("check-circle")
        }

        # Build check rows
        check_rows <- lapply(stk_checks, function(ch) {
          status_cls <- switch(ch$Status,
            "ERROR"   = "check-error",
            "WARNING" = "check-warning",
            "PASS"    = "check-pass"
          )
          div(class = paste("check-row", status_cls),
            div(class = "check-icon",
              switch(ch$Status,
                "ERROR"   = icon("times-circle"),
                "WARNING" = icon("exclamation-triangle"),
                "PASS"    = icon("check-circle")
              )
            ),
            div(class = "check-info",
              div(class = "check-name", ch$Check),
              div(class = "check-rule", ch$Rule),
              if (nchar(ch$Details) > 0) div(class = "check-details", ch$Details)
            )
          )
        })

        div(class = paste("stock-card", stock_class),
          div(class = "stock-card-header",
            stock_icon,
            span(class = "stock-card-name", stk),
            span(class = "stock-card-count", sum(statuses == "PASS"), "/", length(statuses), " passed")
          ),
          div(class = "stock-card-body",
            do.call(tagList, check_rows)
          )
        )
      })

      stock_cards <- Filter(Negate(is.null), stock_cards)
      if (length(stock_cards) == 0) {
        div(class = "empty-state", icon("check-double", class = "fa-3x"), "Tidak ada hasil untuk filter ini")
      } else {
        div(class = "stock-cards-grid", do.call(tagList, stock_cards))
      }
    })

    # Return validation state
    return(list(
      all_passed = reactive({
        !is.null(rv$summary) && rv$summary$error == 0
      }),
      results = reactive(rv$results),
      summary = reactive(rv$summary)
    ))
  })
}
