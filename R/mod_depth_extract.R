#' Depth Extraction Module UI Function
#'
#' @description A shiny Module for depth extraction from raster data
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_depth_extract_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                12,
                shinydashboard::box(
                    title = tags$span(icon("water"), " Depth Extraction"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    status = "primary",
                    width = NULL,
                    solidHeader = TRUE,
                    p("In this section, you can extract depth values for your sampling points from a bathymetry raster file."),
                    p("Upload a raster file (e.g., .tif, .nc, .grd) containing depth/bathymetry data. The depth values will be extracted at each point location and added as a new column called 'depth_m'."),
                    p("Note: Make sure your raster and point data are in the same coordinate reference system for accurate extraction.")
                )
            )
        ),
        fluidRow(
            # File Upload Section
            column(
                4,
                shinydashboard::box(
                    title = "Upload Depth Raster",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    conditionalPanel(
                        condition = sprintf("output['%s'] == false", ns("data_available")),
                        div(
                            style = "text-align: center; padding: 20px;",
                            icon("exclamation-triangle", "fa-2x", style = "color: #f39c12;"),
                            h4("No Point Data", style = "color: #f39c12;"),
                            p("Please complete the Data Input step first.", style = "color: #7f8c8d;")
                        )
                    ),
                    conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("data_available")),
                        h4("Upload Bathymetry Raster"),
                        helpText(tags$span(icon("info-circle"), " Supported formats: GeoTIFF (.tif), NetCDF (.nc), ESRI Grid (.grd), ASCII (.asc).")),
                        fileInput(
                            ns("depth_raster"),
                            "Choose Raster File:",
                            accept = c(".tif", ".tiff", ".nc", ".grd", ".asc")
                        ),
                        br(),
                        fluidRow(
                            column(2),
                            column(
                                4,
                                actionButton(
                                    ns("extract_depth"),
                                    "Extract Depth",
                                    class = "btn-primary btn-block",
                                    icon = icon("water")
                                )
                            ),
                            column(
                                4,
                                actionButton(
                                    ns("clear_results"),
                                    "Clear Results",
                                    class = "btn-danger btn-block",
                                    icon = icon("eraser")
                                )
                            )
                        )
                    )
                )
            ),

            # Results Section
            column(
                8,
                shinydashboard::box(
                    title = "Status",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    htmlOutput(ns("extraction_status")),
                    br(),
                    conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("extraction_complete")),
                        actionButton(
                            ns("proceed_to_model"),
                            "Proceed to Model Application",
                            class = "btn-success",
                            icon = icon("brain")
                        )
                    )
                ),
                shinydashboard::box(
                    title = "Depth Results",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    conditionalPanel(
                        condition = sprintf("output['%s'] == false", ns("extraction_complete")),
                        div(
                            style = "text-align: center; padding: 50px;",
                            icon("water", "fa-3x", style = "color: #ccc;"),
                            h4("No Depth Extracted", style = "color: #ccc;"),
                            p("Upload a raster and click 'Extract Depth' to see results", style = "color: #999;")
                        )
                    ),
                    conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("extraction_complete")),
                        div(
                            h4("Depth Summary"),
                            htmlOutput(ns("depth_summary")),
                            hr(),
                            h4("Data with Depth Values"),
                            DT::DTOutput(ns("depth_table"))
                        )
                    )
                )
            )
        )
    )
}

#' Depth Extraction Module Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param app_data Reactive values object from main app
#'
#' @noRd
#'
mod_depth_extract_server <- function(id, app_data, app_session) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Reactive values for module
        values <- reactiveValues(
            depth_results = NULL,
            extraction_complete = FALSE
        )

        # Check if data is available
        output$data_available <- reactive({
            !is.null(app_data$original_data) && app_data$data_valid
        })
        outputOptions(output, "data_available", suspendWhenHidden = FALSE)

        # Extract depth values
        observeEvent(input$extract_depth, {
            req(input$depth_raster)
            req(app_data$original_data)

            showNotification("Processing depth extraction...", type = "message", duration = 2)


            shinycssloaders::showPageSpinner(
                background = "#cccccccc",
                color = "#333333",
                caption = "Calculating Fetch",
                image = "www/img/insil.gif",
                image.width = "200",
                image.height = "200"
            )


            # Run the depth extraction safely
            result <- tryCatch(
                {
                    extract_depth_values(
                        points_data = app_data$original_data$points,
                        raster_path = input$depth_raster$datapath
                    )
                },
                error = function(e) {
                    showNotification(
                        paste("Error extracting depth:", e$message),
                        type = "error",
                        duration = 5
                    )
                    return(NULL)
                }
            )

            shinycssloaders::hidePageSpinner()

            if (!is.null(result)) {
                # Store depth results separately (don't modify original data)
                values$depth_results <- list(
                    points_with_depth = result,
                    raster_path = input$depth_raster$datapath,
                    extraction_time = Sys.time()
                )
                values$extraction_complete <- TRUE
                
                # Update app data with depth results and metadata
                app_data$depth_results <- values$depth_results
                app_data$depth_extracted <- TRUE
                app_data$depth_timestamp <- Sys.time()
                
                # Store calculation parameters
                app_data$depth_params <- list(
                    raster_file = input$depth_raster$name,
                    n_points_processed = nrow(result),
                    n_points_with_depth = sum(!is.na(result$depth_m))
                )

                showNotification("Depth extraction completed successfully!", type = "message", duration = 3)
            }
        })

        # Clear results
        observeEvent(input$clear_results, {
            # Clear depth-specific results and metadata
            values$depth_results <- NULL
            values$extraction_complete <- FALSE
            clear_calculation_results(app_data, "depth")
            
            showNotification("Depth extraction results cleared.", type = "message", duration = 2)
        })

        # Navigation: Proceed to model application
        observeEvent(input$proceed_to_model, {
            shinydashboard::updateTabItems(session = app_session, inputId = "sidebar", "model_apply")
        })

        # Output: Extraction complete flag
        output$extraction_complete <- reactive({
            values$extraction_complete
        })
        outputOptions(output, "extraction_complete", suspendWhenHidden = FALSE)

        # Output: Depth summary
        output$depth_summary <- renderUI({
            req(values$depth_results)
            req(values$extraction_complete)

            points <- values$depth_results$points_with_depth
            depth_vals <- points$depth_m
            non_na_depth <- sum(!is.na(depth_vals))

            tagList(
                p(strong("Points processed:"), nrow(points)),
                p(strong("Points with depth:"), non_na_depth, sprintf("(%.1f%%)", 100 * non_na_depth / nrow(points))),
                p(strong("Mean depth:"), round(mean(depth_vals, na.rm = TRUE), 2), "m"),
                p(
                    strong("Depth range:"),
                    paste(round(range(depth_vals, na.rm = TRUE), 2), collapse = " - "), "m"
                )
            )
        })

        # Output: Depth results table
        output$depth_table <- DT::renderDT({
            req(values$depth_results)
            req(values$extraction_complete)

            # Convert sf to regular data frame for preview
            depth_data <- sf::st_drop_geometry(values$depth_results$points_with_depth) |>
                dplyr::mutate(
                    depth_m = round(depth_m, 3)
                )

            DT::datatable(
                depth_data,
                options = list(
                    pageLength = 10,
                    dom = "ftip",
                    scrollX = TRUE
                ),
                class = "cell-border stripe"
            )
        })

        # Output: Extraction status
        output$extraction_status <- renderUI({
            if (is.null(app_data$original_data) || !app_data$data_valid) {
                return(p("Please complete data input before extracting depth values."))
            }

            if (values$extraction_complete) {
                return(
                    tagList(
                        div(
                            style = "color: green;",
                            icon("check-circle", "fa-2x"),
                            h4("Extraction Complete!", style = "display: inline; margin-left: 10px;")
                        ),
                        p("Depth values have been successfully extracted and added to your data.")
                    )
                )
            }

            # Default state
            p("Upload a bathymetry raster file and click 'Extract Depth Values' to begin.")
        })

        # Navigation: Proceed to model application
        observeEvent(input$proceed_to_model, {
            shinydashboard::updateTabItems(session = app_session, inputId = "sidebar", "model_apply")
        })
    })
}

# Helper function for depth extraction
extract_depth_values <- function(points_data, raster_path) {
    # Read the raster
    depth_raster <- stars::read_stars(raster_path)

    # Check if CRS match
    points_crs <- sf::st_crs(points_data)
    raster_crs <- sf::st_crs(depth_raster)

    # Transform points if necessary
    points <- points_data
    if (points_crs != raster_crs) {
        points <- sf::st_transform(points_data, sf::st_crs(raster_crs))
    }

    # Extract depth values
    depth_values <- stars::st_extract(depth_raster, points)

    # Add depth values to points data
    if (ncol(depth_values) >= 2) {
        points_data$depth_m <- depth_values[, 1, drop = TRUE]
    } else {
        stop("Could not extract depth values from raster", call. = FALSE)
    }

    return(points_data)
}
