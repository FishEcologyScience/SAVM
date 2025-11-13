#' Input Table Overview Module UI Function
#'
#' @description Displays the assembled modeling dataset so users can
#' track how the input table evolves as they progress through the app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_input_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        conditionalPanel(
          condition = sprintf("output['%s'] == false", ns("data_available")),
          bs4Dash::box(
            title = tags$span(icon("table"), " Input Table"),
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            div(
              style = "text-align: center; padding: 50px;",
              icon("info-circle", "fa-3x", style = "color: #3498db;"),
              h4("Input table not available yet", style = "color: #3498db;"),
              p("Upload data (and optionally calculate fetch/depth) to preview the assembled table.", style = "color: #7f8c8d;")
            )
          )
        ),
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("data_available")),
          bs4Dash::tabBox(
            id = ns("input_tabs"),
            width = NULL,
            title = tags$span(icon("table"), " Input Table Overview"),
            tabPanel(
              title = tagList(icon("info-circle"), "Overview"),
              value = "overview",
              fluidRow(
                column(
                  4,
                  bs4Dash::box(
                    title = "Data Status",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    htmlOutput(ns("input_summary"))
                  )
                ),
                column(
                  8,
                  bs4Dash::box(
                    title = "Columns & Availability",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    htmlOutput(ns("column_summary"))
                  )
                )
              )
            ),
            tabPanel(
              title = tagList(icon("table"), "Data Table"),
              value = "table",
              bs4Dash::box(
                title = "Assembled Input Table",
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                DT::DTOutput(ns("input_table"))
              )
            )
          )
        )
      )
    )
  )
}

#' Input Table Overview Module Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param app_data Reactive values object containing shared app state.
#'
#' @noRd
mod_input_tab_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    assembled_data <- reactive({
      req(app_data$original_data)

      # Establish dependencies so this reacts to downstream calculations
      app_data$fetch_results
      app_data$fetch_calculated
      app_data$depth_results
      app_data$depth_extracted

      tryCatch(
        assemble_modeling_data(app_data),
        error = function(e) {
          showNotification(
            paste("Unable to assemble input table:", e$message),
            type = "error",
            duration = 5
          )
          return(NULL)
        }
      )
    })

    output$data_available <- reactive({
      !is.null(app_data$original_data)
    })
    outputOptions(output, "data_available", suspendWhenHidden = FALSE)

    output$input_summary <- renderUI({
      data <- assembled_data()
      req(data)

      data_summary <- get_data_summary(app_data)
      data_df <- sf::st_drop_geometry(data)

      fetch_na <- if ("fetch_km" %in% names(data_df)) sum(is.na(data_df$fetch_km)) else NA
      depth_na <- if ("depth_m" %in% names(data_df)) sum(is.na(data_df$depth_m)) else NA

      status_badge <- function(condition, label_true, label_false) {
        if (isTRUE(condition)) {
          tags$p(icon("check-circle", style = "color: #27ae60;"), span(label_true))
        } else {
          tags$p(icon("times-circle", style = "color: #c0392b;"), span(label_false))
        }
      }

      tagList(
        p(strong("Rows in assembled table:"), nrow(data_df)),
        status_badge(data_summary$fetch_calculated, "Fetch calculated", "Fetch pending"),
        if (!is.na(fetch_na)) {
          p(icon("info-circle"), strong("Points missing fetch:"), fetch_na)
        },
        status_badge(data_summary$depth_extracted, "Depth extracted", "Depth pending"),
        if (!is.na(depth_na)) {
          p(icon("info-circle"), strong("Points missing depth:"), depth_na)
        },
        status_badge(data_summary$model_applied, "Model applied", "Model pending")
      )
    })

    output$column_summary <- renderUI({
      data <- assembled_data()
      req(data)

      data_df <- sf::st_drop_geometry(data)
      column_list <- names(data_df)

      tagList(
        p(strong("Columns available:"), length(column_list)),
        tags$ul(
          lapply(column_list, function(col) {
            tags$li(col)
          })
        )
      )
    })

    output$input_table <- DT::renderDT({
      data <- assembled_data()
      req(data)

      data_df <- sf::st_drop_geometry(data)

      DT::datatable(
        data_df,
        options = list(
          pageLength = 15,
          dom = "ftip",
          scrollX = TRUE
        ),
        class = "cell-border stripe"
      )
    })
  })
}
