#' Data Input Module UI Function
#'
#' @description A shiny Module for data input and validation
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_data_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        shinydashboard::box(
          title = tags$span(icon("upload"), " Data Input"),
          collapsible = TRUE,
          collapsed = TRUE,
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
          title = "Upload Data",
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
            accept = c(".csv", ".shp", ".geojson", ".gpkg", ".cpg", ".dbf", ".prj", ".sbn", ".sbx", ".xml", ".shx"),
            multiple = TRUE
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
          fluidRow(
            column(2),
            column(
              4,
              actionButton(ns("process_data"), "Process Data", class = "btn-primary btn-block", icon = icon("upload"))
            ),
            column(
              4,
              actionButton(ns("clear_data"), "Clear Data", class = "btn-danger btn-block", icon = icon("eraser"))
            )
          )
        ) # ,
      ),

      # Data Preview Section
      column(
        8,
        shinydashboard::box(
          title = "Status",
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
            # div(
            # style = "text-align: left;",
            actionButton(
              ns("proceed_to_fetch"),
              "Proceed to Fetch Calculation",
              class = "btn-success",
              icon = icon("wind")
            ),
            # br(), br(),
            actionButton(
              ns("proceed_to_model"),
              "Proceed to Model Application",
              class = "btn-info",
              icon = icon("brain")
              # )
            )
          )
        ),
        shinydashboard::box(
          title = "Data Preview",
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
                  # plotOutput(ns("point_plot"), height = "400px")
                  leaflet::leafletOutput(ns("point_map"), height = "400px")
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
#' @param app_data Reactive values object from main app
#'
#' @noRd
#'
mod_data_input_server <- function(id, app_data, app_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for module
    values <- reactiveValues(
      processed_data = NULL,
      validation_results = NULL
    )

    # File processing logic
    observeEvent(input$process_data, {
      req(input$data_file)

      showNotification("Processing data...", type = "message", duration = 2)

      # Run the data processing safely
      result <- tryCatch(
        {
          process_input_data(
            file_path       = input$data_file$datapath,
            data_type       = input$data_source_type,
            grid_spacing    = input$grid_spacing,
            invert          = input$invert_polygon,
            inversion_ratio = input$inversion_ratio,
            crs_input       = input$crs_input,
            crs_output      = input$crs_output
          )
        },
        error = function(e) {
          # Notify user with a clear message
          showNotification(
            paste("Error processing data:", e$message),
            type = "error",
            duration = 5
          )
          # Return NULL so downstream code stops gracefully
          return(NULL)
        }
      )

      # Stop if the process failed
      req(!is.null(result))
      # Store processed data as original data (never modified)
      values$processed_data <- result
      app_data$original_data <- result
      app_data$data_loaded <- TRUE

      showNotification("Data processed successfully!", type = "message", duration = 3)
    })


    # Compute validation once, stored in reactiveVal
    values$validation_results <- reactiveVal(NULL)

    observeEvent(values$processed_data, {
      req(values$processed_data)
      points_data <- values$processed_data$points

      required_cols <- c("longitude", "latitude")
      optional_cols <- c("depth_m", "fetch_km", "secchi", "substrate", "limitation")

      val <- list(
        has_required = all(required_cols %in% names(points_data)),
        available_optional = intersect(optional_cols, names(points_data)),
        missing_optional = setdiff(optional_cols, names(points_data)),
        n_points = nrow(points_data),
        crs = sf::st_crs(points_data)$input
      )

      val$is_valid <- val$has_required && val$n_points > 0
      values$validation_results(val)
      app_data$data_valid <- val$is_valid
    })

    # Clear data
    observeEvent(input$clear_data, {
      # Reset local reactive values
      values$processed_data <- NULL
      values$validation_results(NULL)

      # Reset shared app data and clear all calculations
      app_data$original_data <- NULL
      app_data$data_loaded <- FALSE
      app_data$data_valid <- FALSE

      # Clear all calculation results since original data is gone
      clear_calculation_results(app_data, c("fetch", "depth", "model"))

      # Optional: notify the user
      showNotification("Data cleared successfully.", type = "message", duration = 2)
    })

    # Output: Data valid flag
    output$data_valid <- reactive({
      vals <- values$validation_results()
      !is.null(vals) && vals$is_valid
    })
    outputOptions(output, "data_valid", suspendWhenHidden = FALSE)

    # Output: Data processed flag
    output$data_processed <- reactive({
      !is.null(values$processed_data)
    })
    outputOptions(output, "data_processed", suspendWhenHidden = FALSE)

    # Output: Data summary
    output$data_summary <- renderUI({
      req(values$processed_data)

      points <- values$processed_data$points
      polygon <- values$processed_data$polygon

      file_names <- input$data_file$name
      if (length(file_names) > 1) {
        file_names <- paste(basename(file_names), collapse = ", ")
      }

      tagList(
        p(strong("Points:"), nrow(points), "locations"),
        p(strong("CRS:"), sf::st_crs(points)$input),
        p(strong("Bounds:"), "Polygon defined"),
        p(strong("File(s):"), file_names)
      )
    })

    # Output: Column information
    output$column_info <- renderUI({
      req(values$validation_results())

      val <- values$validation_results()

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
        utils::head(preview_data, 100),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "ftip"
        ),
        class = "cell-border stripe"
      )
    })

    # # Output: Grid preview plot
    # output$point_plot <- renderPlot({
    #   req(values$processed_data)

    #   preview_grid(values$processed_data)
    # })

    output$point_map <- leaflet::renderLeaflet({
      req(values$processed_data)
      pts <- values$processed_data$points
      pol <- values$processed_data$polygon |>
        sf::st_make_valid()

      # Transform if needed
      if (sf::st_crs(pts)$epsg != 4326) {
        pts <- sf::st_transform(pts, 4326)
        pol <- sf::st_transform(pol, 4326)
      }

      # Base map
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addPolygons(data = pol, fillColor = "#a1d99b", fillOpacity = 0.3, color = "#31a354", weight = 2) |>
        leaflet::addCircleMarkers(data = pts, radius = 4, color = "#2c7fb8", fillOpacity = 0.7)
    })

    # Output: Validation status
    output$validation_status <- renderUI({
      if (is.null(values$validation_results())) {
        return(p("Upload and process data to see validation status"))
      }

      val <- values$validation_results()

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
      shinydashboard::updateTabItems(session = app_session, inputId = "sidebar", "fetch_calc")
    })

    # Navigation: Proceed to model application
    observeEvent(input$proceed_to_model, {
      shinydashboard::updateTabItems(session = app_session, inputId = "sidebar", "model_apply")
    })
  })
}


# -----------------------------------------------------------------------------------------
process_input_data <- function(file_path,
                               data_type = c("csv", "spatial_points", "spatial_polygon"),
                               grid_spacing = 500,
                               invert = FALSE,
                               inversion_ratio = 0.5,
                               crs_input = 4326,
                               crs_output = 32617) {
  data_type <- match.arg(data_type)

  # -------------------------------------------------------------
  # Handle multi-file shapefile uploads
  if (length(file_path) > 1) {
    file_path <- filepath_shp(file_path)
  }

  # -------------------------------------------------------------
  # Validate file existence
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }

  # -------------------------------------------------------------
  # check file extension vs. expected type
  ext <- tolower(tools::file_ext(file_path))

  csv_exts <- c("csv")
  spatial_exts <- c("shp", "geojson", "gpkg")

  valid <- switch(data_type,
    csv             = ext %in% csv_exts,
    spatial_points  = ext %in% spatial_exts,
    spatial_polygon = ext %in% spatial_exts,
    FALSE
  )

  if (!valid) {
    stop(
      sprintf(
        "File type mismatch: you selected '%s' but uploaded a '.%s' file.",
        data_type, ext
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------------
  # Handle polygon inversion if requested
  if (data_type == "spatial_polygon" && invert) {
    tryCatch(
      {
        message("Inverting polygon...")
        poly <- sf::st_read(file_path, quiet = TRUE)
        poly_inv <- invert_polygon(poly, ratio = inversion_ratio)
        message("Polygon inversion complete.")
        tmp_file <- tempfile(fileext = ".gpkg")
        sf::st_write(poly_inv, tmp_file, quiet = TRUE)
        file_path <- tmp_file
      },
      error = function(e) {
        stop("Failed to invert polygon: ", e$message, call. = FALSE)
      }
    )
  }

  # -------------------------------------------------------------
  # Read and process data (using provided parameters directly)
  tryCatch(
    {
      result <- read_sav(
        file_path = file_path,
        spacing   = grid_spacing,
        crs       = crs_output,
        crs_input = crs_input
      )
    },
    error = function(e) {
      stop("Failed to read or process data: ", e$message, call. = FALSE)
    }
  )

  # Add id_point column to points data
  result$points <- result$points |>
    dplyr::mutate(id_point = dplyr::row_number())

  # Basic structural validation
  if (is.null(result$points) || nrow(result$points) == 0) {
    stop("No point data produced - check input format or CRS parameters.", call. = FALSE)
  }

  result
}


filepath_shp <- function(file_path) {
      shp_idx <- grep("\\.shp$", file_path, ignore.case = TRUE)

    if (length(shp_idx) == 1) {
      # Define a consistent base name
      base_dir <- dirname(file_path[shp_idx])
      new_base <- file.path(base_dir, "uploaded_shapefile")

      # Rename all shapefile components to have the same basename
      for (f in file_path) {
        ext <- tools::file_ext(f)
        file.rename(f, file.path(base_dir, paste0("uploaded_shapefile.", ext)))
      }
      print(dir(base_dir))
      # Use the .shp file for reading
      file_path <- paste0(new_base, ".shp")
    } else if (length(shp_idx) == 0) {
      stop(
        "Multiple files uploaded, but none have a .shp extension.
      Include the .shp, .dbf, .shx, and .prj files together.",
        call. = FALSE
      )
    } else {
      stop(
        "Multiple .shp files detected - please upload only one shapefile at a time.",
        call. = FALSE
      )
    }

    return(file_path)
}