#' Fetch Calculation Module UI Function
#'
#' @description A shiny Module for wind fetch calculation
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_fetch_calc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        bs4Dash::box(
          title = tags$span(icon("wind"), " Fetch Calculation"),
          collapsible = TRUE,
          collapsed = TRUE,
          status = "primary",
          width = NULL,
          solidHeader = TRUE,
          p("In this part of the application, you will calculate the fetch of the points you created in the data input phase")
        )
      )
    ),
    fluidRow(
      # Configuration Panel
      column(
        4,
        # -----------------
        # Parameters
        bs4Dash::box(
          title = "Fetch Parameters",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          # Polygon for fetch calculation
          h4("Choose Spatial Polygon"),
          shinyWidgets::prettySwitch(
            inputId = ns("use_polygon_upload"),
            label = "Upload from file",
            value = FALSE,
            status = "primary",
            inline = TRUE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == false", ns("use_polygon_upload")),
            selectInput(
              ns("polygon_library"),
              "Available polygons:",
              choices = NULL,
              selected = NULL
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("use_polygon_upload")),
            helpText(tags$span(icon("info-circle"), " Supported formats: GeoPackage (.gpkg), GeoJSON (.geojson), ESRI Shapefile (.shp)")),
            fileInput(
              ns("aoi_polygon"),
              "Choose Spatial File:",
              accept = c(".shp", ".geojson", ".gpkg", ".cpg", ".dbf", ".prj", ".sbn", ".sbx", ".xml", ".shx"),
              multiple = TRUE
            )
          ),

          # Parameters
          h4("Main parameters"),
          numericInput(
            ns("max_dist"),
            "Maximum fetch distance (km):",
            value = 15,
            min = 1,
            max = 100,
            step = 1
          ),
          numericInput(
            ns("n_bearings"),
            "Number of bearings:",
            value = 16,
            min = 4,
            max = 64,
            step = 1
          ),

          # -----------------
          # Wind weights
          h4("Wind weights (optional)"),
          checkboxInput(
            ns("use_wind_weights"),
            "Use custom wind weights",
            value = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("use_wind_weights")),
            helpText(tags$span(icon("info-circle"), " CSV must contain 'direction' (0-360 degrees) and 'weight' columns.")),
            fileInput(
              ns("wind_weights_file"),
              "Upload wind weights CSV:",
              accept = ".csv"
            ),
            conditionalPanel(
              condition = sprintf("output['%s'] == true", ns("wind_weights_valid")),
              h5("Wind Weights Preview:"),
              DT::DTOutput(ns("wind_weights_preview"))
            )
          ),
          br(),
          fluidRow(
            column(2),
            column(
              4,
              actionButton(ns("calculate_fetch"), "Calculate Fetch", class = "btn-primary btn-block", icon = icon("wind"))
            ),
            column(
              4,
              actionButton(ns("clear_results"), "Clear Results", class = "btn-danger btn-block", icon = icon("eraser"))
            )
          )
        ),
      ),

      # Results Panel
      column(
        8,
        # Status Panel
        bs4Dash::box(
          title = "Status",
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          htmlOutput(ns("status_message")),
          br(),
          conditionalPanel(
            condition = sprintf("output['%s'] == true", ns("fetch_calculated")),
            actionButton(
              ns("proceed_to_depth"),
              "Proceed to Depth Extraction",
              class = "btn-success",
              icon = icon("water")
            ),
            actionButton(
              ns("proceed_to_model"),
              "Proceed to Model Application",
              class = "btn-info",
              icon = icon("brain")
            )
          )
        ),
        bs4Dash::box(
          title = "Fetch Results",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          conditionalPanel(
            condition = sprintf("output['%s'] == false", ns("fetch_calculated")),
            div(
              style = "text-align: center; padding: 50px;",
              icon("wind", "fa-3x", style = "color: #ccc;"),
              h4("No Fetch Calculated", style = "color: #ccc;"),
              p("Configure parameters and click 'Calculate Fetch' to see results", style = "color: #999;")
            )
          ),
          conditionalPanel(
            condition = sprintf("output['%s'] == true", ns("fetch_calculated")),
            div(
              fluidRow(
                column(
                  6,
                  h4("Fetch Summary"),
                  htmlOutput(ns("fetch_summary"))
                ),
                column(
                  6,
                  h4("Fetch Visualization"),
                  leaflet::leafletOutput(ns("fetch_map"), height = "400px")
                )
              ),
              hr(),
              h4("Fetch Results Table"),
              DT::DTOutput(ns("fetch_table"))
            )
          )
        )
      )
    )
  )
}

#' Fetch Calculation Module Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param app_data Reactive values object from main app
#' @param app_session Main app session for navigation
#'
#' @noRd
#'
mod_fetch_calc_server <- function(id, app_data, app_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for module
    values <- reactiveValues(
      wind_weights = NULL,
      fetch_results = NULL,
      polygon_data = NULL
    )

    # Initialize polygon library choices
    observe({
      polygon_files <- list.files(
        system.file("extdata", "polygons", package = "SAVM"),
        pattern = "\\.(gpkg|geojson)$",
        full.names = FALSE
      )
      polygon_choices <- stats::setNames(polygon_files, tools::file_path_sans_ext(polygon_files))

      updateSelectInput(
        session,
        "polygon_library",
        choices = polygon_choices,
        selected = if (length(polygon_choices) > 0) polygon_choices[1] else NULL
      )
    })

    # # Handle polygon selection from library
    # observeEvent(input$polygon_library, {
    #   req(input$polygon_library)
    #   req(!input$use_polygon_upload)

    #   tryCatch(
    #     {
    #       polygon_path <- system.file("extdata", "polygons", input$polygon_library, package = "SAVM")
    #       polygon_data <- sf::st_read(polygon_path, quiet = TRUE)
    #       values$polygon_data <- polygon_data
    #       showNotification("Polygon loaded from library", type = "message", duration = 3)
    #     },
    #     error = function(e) {
    #       showNotification(
    #         paste("Error loading polygon from library:", e$message),
    #         type = "error",
    #         duration = 5
    #       )
    #       values$polygon_data <- NULL
    #     }
    #   )
    # })

    # # Handle uploaded polygon file
    # observeEvent(input$aoi_polygon, {
    #   req(input$aoi_polygon)
    #   req(input$use_polygon_upload)

    #   tryCatch(
    #     {
    #       polygon_data <- sf::st_read(input$aoi_polygon$datapath, quiet = TRUE)
    #       values$polygon_data <- polygon_data
    #       showNotification("Polygon uploaded successfully", type = "message", duration = 3)
    #     },
    #     error = function(e) {
    #       showNotification(
    #         paste("Error reading uploaded polygon:", e$message),
    #         type = "error",
    #         duration = 5
    #       )
    #       values$polygon_data <- NULL
    #     }
    #   )
    # })

    # Wind weights file processing
    observeEvent(input$wind_weights_file, {
      req(input$wind_weights_file)

      tryCatch(
        {
          wind_data <- utils::read.csv(input$wind_weights_file$datapath, stringsAsFactors = FALSE)

          # Validate required columns
          if (!all(c("direction", "weight") %in% names(wind_data))) {
            showNotification(
              "Wind weights file must contain 'direction' and 'weight' columns",
              type = "error",
              duration = 5
            )
            values$wind_weights <- NULL
            return()
          }

          # Validate direction range
          if (!all(wind_data$direction >= 0 & wind_data$direction <= 360)) {
            showNotification(
              "All directions must be between 0 and 360 degrees",
              type = "error",
              duration = 5
            )
            values$wind_weights <- NULL
            return()
          }

          values$wind_weights <- wind_data
          showNotification("Wind weights loaded successfully", type = "message", duration = 3)
        },
        error = function(e) {
          showNotification(
            paste("Error reading wind weights file:", e$message),
            type = "error",
            duration = 5
          )
          values$wind_weights <- NULL
        }
      )
    })

    # Wind weights validation flag
    output$wind_weights_valid <- reactive({
      !is.null(values$wind_weights)
    })
    outputOptions(output, "wind_weights_valid", suspendWhenHidden = FALSE)

    # Wind weights preview table
    output$wind_weights_preview <- DT::renderDT({
      req(values$wind_weights)

      DT::datatable(
        utils::head(values$wind_weights, 20),
        options = list(
          pageLength = 5,
          dom = "t",
          scrollX = TRUE
        ),
        class = "cell-border stripe"
      )
    })

    # Fetch calculation

    observeEvent(input$calculate_fetch, {
      req(app_data$original_data)
      req(app_data$data_valid)

      tryCatch(
        {
          showNotification("Processing polygon...", type = "message", duration = 2)

          # Select polygon source
          if (isTRUE(input$use_polygon_upload)) {
            req(input$aoi_polygon)

            # Handle multiple shapefile components
            file_paths <- input$aoi_polygon$datapath
            if (length(file_paths) > 1) {
              polygon_file <- filepath_shp(file_paths)
            } else {
              polygon_file <- file_paths
            }

            polygon <- sf::st_read(polygon_file, quiet = TRUE)
          } else {
            req(input$polygon_library)

            polygon_file <- system.file(
              "extdata", "polygons", input$polygon_library,
              package = "SAVM"
            )
            polygon <- sf::st_read(polygon_file, quiet = TRUE)
          }

          # Store for later use
          values$polygon_data <- polygon

          showNotification("Calculating fetch...", type = "message", duration = 2)

          # Extract data from original data
          points <- app_data$original_data$points

          # Check if polygon is available
          if (is.null(polygon)) {
            showNotification("Please select or upload a polygon first", type = "error", duration = 5)
            return()
          }

          # Prepare wind weights if using custom
          wind_weights <- if (input$use_wind_weights && !is.null(values$wind_weights)) {
            values$wind_weights
          } else {
            NULL
          }

          # Calculate fetch
          shinycssloaders::showPageSpinner(
            background = "#cccccccc",
            color = "#333333",
            caption = "Calculating Fetch",
            image = "www/img/insil.gif",
            image.width = "200",
            image.height = "200"
          )
          fetch_result <- compute_fetch(
            points = points,
            polygon = polygon,
            max_dist = input$max_dist,
            n_bearings = input$n_bearings,
            wind_weights = wind_weights
          )
          shinycssloaders::hidePageSpinner()

          # Store results separately (don't modify original data)
          values$fetch_results <- fetch_result

          # Update app data with fetch results and metadata
          app_data$fetch_results <- fetch_result
          app_data$fetch_calculated <- TRUE
          app_data$fetch_timestamp <- Sys.time()

          # Store calculation parameters for reference
          app_data$fetch_params <- list(
            max_dist = input$max_dist,
            n_bearings = input$n_bearings,
            used_wind_weights = !is.null(wind_weights)
          )

          showNotification("Fetch calculation completed successfully!", type = "message", duration = 3)
        },
        error = function(e) {
          showNotification(
            paste("Error calculating fetch:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    })

    # Clear results
    observeEvent(input$clear_results, {
      # Clear fetch-specific results and metadata
      values$fetch_results <- NULL
      clear_calculation_results(app_data, "fetch")

      showNotification("Fetch results cleared", type = "message", duration = 2)
    })

    # Fetch calculated flag
    output$fetch_calculated <- reactive({
      !is.null(values$fetch_results)
    })
    outputOptions(output, "fetch_calculated", suspendWhenHidden = FALSE)

    # Fetch summary
    output$fetch_summary <- renderUI({
      req(values$fetch_results)

      fetch_data <- values$fetch_results$mean_fetch |> sf::st_drop_geometry()

      tagList(
        p(strong("Points processed:"), nrow(fetch_data)),
        p(strong("Mean fetch:"), round(mean(fetch_data$fetch_km, na.rm = TRUE), 2), "km"),
        p(strong("Mean weighted fetch:"), round(mean(fetch_data$weighted_fetch_km, na.rm = TRUE), 2), "km"),
        p(
          strong("Fetch range:"),
          paste(round(range(fetch_data$fetch_km, na.rm = TRUE), 2), collapse = " - "), "km"
        )
      )
    })

    output$fetch_map <- leaflet::renderLeaflet({
      req(values$fetch_results)

      pts <- values$fetch_results$mean_fetch |>
        sf::st_make_valid()
      transects <- values$fetch_results$transect_lines |>
        sf::st_make_valid()

      # Transform if needed
      if (sf::st_crs(pts)$epsg != 4326) {
        pts <- sf::st_transform(pts, 4326)
        transects <- sf::st_transform(transects, 4326)
      }

      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addPolylines(data = transects, color = "red", weight = 1, group = "Transects") |>
        leaflet::addCircleMarkers(data = pts, color = "blue", radius = 4, group = "Mean fetch") |>
        leaflet::addLayersControl(
          overlayGroups = c("Transects", "Mean fetch"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        )
    })
    # Fetch results table
    output$fetch_table <- DT::renderDT({
      req(values$fetch_results)

      fetch_data <- values$fetch_results$mean_fetch |>
        sf::st_drop_geometry() |>
        dplyr::mutate(
          fetch_km = round(fetch_km, 3),
          weighted_fetch_km = round(weighted_fetch_km, 3)
        )

      DT::datatable(
        fetch_data,
        options = list(
          pageLength = 10,
          dom = "ftip",
          scrollX = TRUE
        ),
        class = "cell-border stripe"
      )
    })

    # Status message
    output$status_message <- renderUI({
      if (is.null(app_data$original_data)) {
        p(
          icon("exclamation-triangle", style = "color: orange;"),
          "No data available. Please complete the Data Input step first."
        )
      } else if (is.null(values$fetch_results)) {
        p(
          icon("info-circle", style = "color: blue;"),
          "Configure parameters and calculate fetch to proceed."
        )
      } else {
        p(
          icon("check-circle", style = "color: green;"),
          "Fetch calculation completed successfully. You can proceed to the next step."
        )
      }
    })

    # Navigation: Proceed to depth extraction
    observeEvent(input$proceed_to_depth, {
      bs4Dash::updateTabItems(session = app_session, inputId = "sidebar", "depth_extr")
    })

    # Navigation: Proceed to model application
    observeEvent(input$proceed_to_model, {
      bs4Dash::updateTabItems(session = app_session, inputId = "sidebar", "model_apply")
    })
  })
}
