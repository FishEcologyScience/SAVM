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
      # Configuration Panel
      column(
        4,
        bs4Dash::box(
          title = tags$span(icon("wind"), " Fetch Calculation"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          conditionalPanel(
            condition = sprintf("output['%s'] == false", ns("data_available")),
            div(
              style = "text-align: center; padding: 20px;",
              icon("exclamation-triangle", "fa-2x", style = "color: #f39c12;"),
              h4("No Data Avaliable", style = "color: #f39c12;"),
              p("Please complete the Data Input step first.", style = "color: #7f8c8d;")
            )
          ),
          conditionalPanel(
            condition = sprintf("output['%s'] == true", ns("data_available")),
            # Polygon for fetch calculation
            h5(strong("Choose Spatial Polygon")),
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
              shp_help_text(),
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
            shinyWidgets::prettySwitch(
              inputId = ns("use_wind_weights"),
              label = "Use custom wind weights",
              value = FALSE,
              status = "primary",
              inline = TRUE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_wind_weights")),
              h6(strong("Upload wind weights CSV")),
              helpText(tags$span(style = "color: #6c757d;", icon("info-circle"), " CSV must contain 'direction' (0-360 degrees) and 'weight' columns.")),
              template_download_button(ns("download_wind_weight_template"), "Wind Weights Template"),
              fileInput(
                ns("wind_weights_file"),
                NULL,
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
          )
        )
      ),

      # Results Panel
      column(
        8,
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

    # Check if data is available
    output$data_available <- reactive({
      !is.null(app_data$original_data) && app_data$data_valid
    })
    outputOptions(output, "data_available", suspendWhenHidden = FALSE)

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

    # Download handlers for CSV and wind weight template
    output$download_wind_weight_template <- downloadHandler(
      filename = function() {
        "sav_wind_weights_template.csv"
      },
      content = function(file) {
        template_path <- system.file("extdata", "templates", "wind_weights_template.csv", package = "SAVM")
        file.copy(template_path, file)
      }
    )

    # Fetch calculation
    observeEvent(input$calculate_fetch, {
      req(app_data$original_data)
      req(app_data$data_valid)


      # Calculate fetch
      shinycssloaders::showPageSpinner(
        background = "#cccccccc",
        color = "#333333",
        caption = "Calculating Fetch",
        image = "www/img/insil.gif",
        image.width = "200",
        image.height = "200"
      )

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

            polygon <- sf::st_read(polygon_file, quiet = TRUE) |>
              sf::st_make_valid() |>
              sf::st_zm()
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

          fetch_result <- compute_fetch(
            points = points,
            polygon = polygon,
            max_dist = input$max_dist,
            n_bearings = input$n_bearings,
            wind_weights = wind_weights
          )

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
      shinycssloaders::hidePageSpinner()
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
      valid_fetch <- fetch_data |> dplyr::filter(!is.na(fetch_km))
      na_count <- sum(is.na(fetch_data$fetch_km))

      mean_fetch <- if (nrow(valid_fetch) > 0) {
        paste0(round(mean(valid_fetch$fetch_km), 2), " km")
      } else {
        "Not available"
      }

      weighted_mean <- if (nrow(valid_fetch) > 0) {
        paste0(round(mean(valid_fetch$weighted_fetch_km), 2), " km")
      } else {
        "Not available"
      }

      fetch_range <- if (nrow(valid_fetch) > 0) {
        rng <- range(valid_fetch$fetch_km)
        paste0(round(rng[1], 2), " - ", round(rng[2], 2), " km")
      } else {
        "Not available"
      }

      tagList(
        p(strong("Points processed:"), nrow(fetch_data)),
        p(strong("Points outside polygon (no fetch):"), na_count),
        p(strong("Mean fetch:"), mean_fetch),
        p(strong("Mean weighted fetch:"), weighted_mean),
        p(strong("Fetch range:"), fetch_range)
      )
    })

    output$fetch_map <- leaflet::renderLeaflet({
      req(values$fetch_results)

      pts <- values$fetch_results$mean_fetch |> sf::st_make_valid()
      pts <- pts |>
        dplyr::mutate(
          fetch_color = dplyr::if_else(is.na(fetch_km), "#d73027", "#1f78b4"),
          fetch_label = dplyr::if_else(
            is.na(fetch_km),
            "Fetch unavailable (outside polygon)",
            paste0("Fetch: ", round(fetch_km, 2), " km")
          )
        )

      transects <- values$fetch_results$transect_lines
      polygon <- values$polygon_data

      # Ensure geometries are valid
      if (!is.null(transects)) {
        transects <- sf::st_make_valid(transects)
      }
      if (!is.null(polygon)) {
        polygon <- sf::st_make_valid(polygon)
      }

      target_crs <- 4326
      if (!is.null(sf::st_crs(pts)) && !identical(sf::st_crs(pts)$epsg, target_crs)) {
        pts <- sf::st_transform(pts, target_crs)
      }
      if (!is.null(transects) && !is.null(sf::st_crs(transects)) &&
        !identical(sf::st_crs(transects)$epsg, target_crs)) {
        transects <- sf::st_transform(transects, target_crs)
      }
      if (!is.null(polygon) && !is.null(sf::st_crs(polygon)) &&
        !identical(sf::st_crs(polygon)$epsg, target_crs)) {
        polygon <- sf::st_transform(polygon, target_crs)
      }

      overlay_groups <- c("Points")
      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron")

      if (!is.null(polygon)) {
        map <- map |>
          leaflet::addPolygons(
            data = polygon,
            fillColor = "#a6cee3",
            fillOpacity = 0.2,
            color = "#1f78b4",
            weight = 2,
            group = "Polygon"
          )
        overlay_groups <- c(overlay_groups, "Polygon")
      }

      if (!is.null(transects)) {
        map <- map |>
          leaflet::addPolylines(
            data = transects,
            color = "#fb6a4a",
            weight = 1,
            group = "Transects"
          )
        overlay_groups <- c(overlay_groups, "Transects")
      }

      map |>
        leaflet::addCircleMarkers(
          data = pts,
          color = ~fetch_color,
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.9,
          label = ~fetch_label,
          group = "Points"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = overlay_groups,
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
  })
}
