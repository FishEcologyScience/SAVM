#' Data Input Module UI Function
#'
#' @description A shiny Module for data input and validation
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny plotOutput renderPlot
mod_data_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        shinydashboard::box(
          title = tags$span(icon("upload"), " Data Input"),
          collapsible = TRUE,
          status = "primary",
          width = NULL,
          solidHeader = TRUE,
          p("In this part of the application, you will upload your data to apply the SAVM modelling framework."),
          p("You can provide your data as sampling point data in tabular (.csv) or spatial (e.g. .gpkg, .geojson, .shp) format that can be directly used for modelling. This dataset must contain the coordinates of your sampling points and optionally include fetch, depth, substrate, secchi and limitation."),
          p("Alternatively, you can provide your data as a spatial polygon representing your area of interest, from which we will create a regular point grid using a user-specified spacing parameter.")
        )
      )
    ),
    fluidRow(
      # File Upload Section
      column(
        4,
        shinydashboard::box(
          title = "1. Upload Data",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          h4("Select Data Source"),
          selectInput(
            ns("data_source_type"),
            "Data Type:",
            choices = list(
              "Point Data (CSV)" = "csv",
              "Point Data (Spatial)" = "spatial_points",
              "Area of Interest (Polygon)" = "spatial_polygon"
            ),
            selected = "csv"
          ),
          fileInput(
            ns("data_file"),
            "Choose File:",
            accept = c(".csv", ".shp", ".geojson", ".gpkg", ".gdb")
          ),

          # Conditional inputs based on data type
          conditionalPanel(
            condition = sprintf("input['%s'] == 'csv'", ns("data_source_type")),
            h5("CSV Configuration"),
            numericInput(
              ns("crs_input"),
              "Input CRS (EPSG):",
              value = 4326,
              min = 1,
              max = 99999
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'spatial_polygon'", ns("data_source_type")),
            h5("Grid Generation"),
            numericInput(
              ns("grid_spacing"),
              "Grid Spacing (meters):",
              value = 500,
              min = 10,
              max = 10000,
              step = 50
            ),
            br(),
            checkboxInput(
              ns("invert_polygon"),
              "Invert polygon (land surrounding water of interest)",
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("invert_polygon")),
              numericInput(
                ns("inversion_ratio"),
                "Inversion ratio:",
                value = 0.5,
                min = 0.01,
                max = 1.0,
                step = 0.01
              ),
              helpText(tags$span(icon("question-circle"), " Ratio for concave hull generation. Lower values create tighter hulls around land features."))
            )
          ),
          numericInput(
            ns("crs_output"),
            "Output CRS (EPSG):",
            value = 32617,
            min = 1,
            max = 99999
          ),
          br(),
          actionButton(
            ns("process_data"),
            "Process Data",
            class = "btn-primary btn-block",
            icon = icon("play")
          )
        ),
        shinydashboard::box(
          title = "3. Data Validation Status",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          htmlOutput(ns("validation_status")),
          br(),
          conditionalPanel(
            condition = sprintf(
              "output['%s'] == true && output['%s'] == true",
              ns("data_processed"), ns("data_valid")
            ),
            div(
              style = "text-align: left;",
              actionButton(
                ns("proceed_to_fetch"),
                "Proceed to Fetch Calculation",
                class = "btn-success btn-lg",
                icon = icon("arrow-right")
              ),
              br(), br(),
              actionButton(
                ns("skip_to_model"),
                "Skip to Model Application",
                class = "btn-warning",
                icon = icon("forward")
              )
            )
          )
        )
      ),

      # Data Preview Section
      column(
        8,
        shinydashboard::box(
          title = "2. Data Preview",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          conditionalPanel(
            condition = sprintf("output['%s'] == false", ns("data_processed")),
            div(
              style = "text-align: center; padding: 50px;",
              icon("upload", "fa-3x", style = "color: #ccc;"),
              h4("No Data Loaded", style = "color: #ccc;"),
              p("Upload a file and click 'Process Data' to see preview", style = "color: #999;")
            )
          ),
          conditionalPanel(
            condition = sprintf("output['%s'] == true", ns("data_processed")),
            div(
              fluidRow(
                # Data Summary & Column Info
                column(
                  6,
                  h4("Data Summary"),
                  htmlOutput(ns("data_summary")),
                  hr(),
                  h4("Column Information"),
                  htmlOutput(ns("column_info"))
                ),
                # Grid Preview
                column(
                  6,
                  h4("Point Locations"),
                  plotOutput(ns("point_plot"), height = "400px")
                )
              ),
              hr(),
              # Full width - Data Preview (unchanged)
              h4("Data Preview (first 100 rows)"),
              DT::DTOutput(ns("data_preview"))
            )
          )
        )
      )
    )
  )
}

#' Data Input Module Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param app_values Reactive values object from main app
#'
#' @noRd
#'
mod_data_input_server <- function(id, app_values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for module
    values <- reactiveValues(
      raw_data = NULL,
      processed_data = NULL,
      validation_results = NULL
    )

    # File processing logic
    observeEvent(input$process_data, {
      req(input$data_file)

      tryCatch(
        {
          showNotification("Processing data...", type = "message", duration = 2)

          # Determine parameters based on data type
          file_path <- input$data_file$datapath
          spacing <- if (input$data_source_type == "spatial_polygon") input$grid_spacing else 500
          crs_input <- if (input$data_source_type == "csv") input$crs_input else 4326

          # Handle polygon inversion if needed
          if (input$data_source_type == "spatial_polygon" && input$invert_polygon) {
            showNotification("Inverting polygon...", type = "message", duration = 2)

            # Read the original polygon
            original_polygon <- sf::st_read(file_path, quiet = TRUE)

            # Apply inversion
            inverted_polygon <- invert_polygon(original_polygon, ratio = input$inversion_ratio)

            # Create temporary file for inverted polygon
            temp_dir <- tempdir()
            temp_file <- file.path(temp_dir, paste0("inverted_polygon_", Sys.time() |> as.numeric(), ".gpkg"))

            # Save inverted polygon to temporary file
            sf::st_write(inverted_polygon, temp_file, quiet = TRUE)

            # Use temporary file path
            file_path <- temp_file

            showNotification("Polygon inverted successfully!", type = "success", duration = 2)
          }

          # Call SAVM read_sav function
          result <- read_sav(
            file_path = file_path,
            spacing = spacing,
            crs = input$crs_output,
            crs_input = crs_input
          )

          values$processed_data <- result
          app_values$sav_data <- result
          app_values$data_loaded <- TRUE

          showNotification("Data processed successfully!", type = "success", duration = 3)
        },
        error = function(e) {
          showNotification(
            paste("Error processing data:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # Data validation
    validation_results <- reactive({
      req(values$processed_data)

      points_data <- values$processed_data$points

      # Check for required columns
      required_cols <- c("longitude", "latitude")
      optional_cols <- c("depth_m", "fetch_km", "secchi", "substrate", "limitation")

      has_required <- all(required_cols %in% names(points_data))
      available_optional <- intersect(optional_cols, names(points_data))
      missing_optional <- setdiff(optional_cols, names(points_data))

      # Additional validation
      n_points <- nrow(points_data)
      crs_info <- sf::st_crs(points_data)

      list(
        is_valid = has_required && n_points > 0,
        n_points = n_points,
        has_required = has_required,
        available_optional = available_optional,
        missing_optional = missing_optional,
        crs = crs_info$input
      )
    })

    # Output: Data processed flag
    output$data_processed <- reactive({
      !is.null(values$processed_data)
    })
    outputOptions(output, "data_processed", suspendWhenHidden = FALSE)

    # Output: Data valid flag
    output$data_valid <- reactive({
      if (is.null(validation_results())) {
        return(FALSE)
      }
      validation_results()$is_valid
    })
    outputOptions(output, "data_valid", suspendWhenHidden = FALSE)

    # Output: Data summary
    output$data_summary <- renderUI({
      req(values$processed_data)

      points <- values$processed_data$points
      polygon <- values$processed_data$polygon

      tagList(
        p(strong("Points:"), nrow(points), "locations"),
        p(strong("CRS:"), sf::st_crs(points)$input),
        p(strong("Bounds:"), "Polygon defined"),
        p(strong("File:"), input$data_file$name)
      )
    })

    # Output: Column information
    output$column_info <- renderUI({
      req(validation_results())

      val <- validation_results()

      tagList(
        if (val$has_required) {
          p(
            icon("check", style = "color: green;"),
            strong("Required columns present:"), "longitude, latitude"
          )
        } else {
          p(
            icon("times", style = "color: red;"),
            strong("Missing required columns")
          )
        },
        if (length(val$available_optional) > 0) {
          p(
            icon("check", style = "color: green;"),
            strong("Optional columns available:"),
            paste(val$available_optional, collapse = ", ")
          )
        },
        if (length(val$missing_optional) > 0) {
          p(
            icon("info-circle", style = "color: orange;"),
            strong("Optional columns missing:"),
            paste(val$missing_optional, collapse = ", ")
          )
        }
      )
    })

    # Output: Data preview table
    output$data_preview <- DT::renderDT({
      req(values$processed_data)

      # Convert sf to regular data frame for preview
      preview_data <- sf::st_drop_geometry(values$processed_data$points)

      DT::datatable(
        head(preview_data, 100),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "ftip"
        ),
        class = "cell-border stripe"
      )
    })

    # Output: Grid preview plot
    output$point_plot <- renderPlot({
      req(values$processed_data)

      preview_grid(values$processed_data)
    })

    # Output: Validation status
    output$validation_status <- renderUI({
      if (is.null(validation_results())) {
        return(p("Upload and process data to see validation status"))
      }

      val <- validation_results()

      if (val$is_valid) {
        tagList(
          div(
            style = "color: green;",
            icon("check-circle", "fa-2x"),
            h4("Data Valid!", style = "display: inline; margin-left: 10px;")
          ),
          p("Your data meets all requirements and is ready for analysis."),
          p(strong("Summary:"), val$n_points, "points loaded successfully")
        )
      } else {
        tagList(
          div(
            style = "color: red;",
            icon("exclamation-triangle", "fa-2x"),
            h4("Data Invalid", style = "display: inline; margin-left: 10px;")
          ),
          p("Please check the data requirements and upload a valid file.")
        )
      }
    })

    # Navigation: Proceed to fetch calculation
    observeEvent(input$proceed_to_fetch, {
      shinydashboard::updateTabItems(session = session$parent, "sidebar", "fetch_calc")
    })

    # Navigation: Skip to model application
    observeEvent(input$skip_to_model, {
      shinydashboard::updateTabItems(session = session$parent, "sidebar", "model_apply")
    })
  })
}
