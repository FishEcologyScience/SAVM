#' Results Visualization Module UI Function
#'
#' @description A shiny Module for visualizing SAV model results
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_results_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        conditionalPanel(
          condition = sprintf("output['%s'] == false", ns("results_available")),
          bs4Dash::box(
            title = tags$span(icon("chart-line"), " Results & Visualization"),
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            div(
              style = "text-align: center; padding: 50px;",
              icon("exclamation-triangle", "fa-3x", style = "color: #f39c12;"),
              h4("No Model Results Found", style = "color: #f39c12;"),
              p("Please complete the previous steps to generate results for visualization:", style = "color: #7f8c8d;"),
              tags$ul(
                style = "color: #7f8c8d; text-align: left; display: inline-block;",
                tags$li("1. Upload your data"),
                tags$li("2. Calculate fetch (optional)"),
                tags$li("3. Extract depth (optional)"),
                tags$li("4. Apply SAV models")
              )
            )
          )
        ),
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("results_available")),
          bs4Dash::tabBox(
            id = ns("viz_tabs"),
            width = NULL,
            title = tags$span(icon("chart-line"), " Results & Visualization"),

            # Summary Tab
            tabPanel(
              title = tagList(icon("info-circle"), "Summary"),
              value = "summary",
              fluidRow(
                column(
                  6,
                  bs4Dash::box(
                    title = "Data Overview",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    htmlOutput(ns("data_summary"))
                  )
                ),
                column(
                  6,
                  bs4Dash::box(
                    title = "Model Results Summary",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    htmlOutput(ns("model_summary"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  bs4Dash::box(
                    title = "Available Predictors & Results",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    htmlOutput(ns("predictors_summary"))
                  )
                )
              )
            ),

            # Distribution Plots Tab
            tabPanel(
              title = tagList(icon("bar-chart"), "Distribution"),
              value = "distribution",
              fluidRow(
                column(
                  3,
                  bs4Dash::box(
                    title = "Plot Parameters",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    checkboxGroupInput(
                      ns("dist_type"),
                      "Prediction Type:",
                      choices = list(
                        "Presence/Absence" = "pa",
                        "Cover" = "cover"
                      ),
                      selected = c("pa", "cover")
                    ),
                    checkboxGroupInput(
                      ns("dist_predictors"),
                      "Predictors:",
                      choices = list(
                        "Depth" = "depth",
                        "Fetch" = "fetch"
                      ),
                      selected = c("depth", "fetch")
                    ),
                    checkboxInput(
                      ns("dist_post_hoc"),
                      "Use post-hoc results",
                      value = TRUE
                    ),
                    numericInput(
                      ns("dist_max_depth"),
                      "Max depth bin (m):",
                      value = 30,
                      min = 5,
                      max = 100,
                      step = 5
                    ),
                    numericInput(
                      ns("dist_max_fetch"),
                      "Max fetch bin (km):",
                      value = 15,
                      min = 1,
                      max = 50,
                      step = 1
                    ),
                    br(),
                    actionButton(
                      ns("update_dist_plot"),
                      "Update Plot",
                      class = "btn-primary btn-block",
                      icon = icon("refresh")
                    ),
                    br(),
                    downloadButton(
                      ns("download_dist_plot"),
                      "Download Plot",
                      class = "btn-success btn-block",
                      icon = icon("download")
                    )
                  )
                ),
                column(
                  9,
                  bs4Dash::box(
                    title = "Distribution Plots",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    conditionalPanel(
                      condition = sprintf("output['%s'] == false", ns("dist_plot_available")),
                      div(
                        style = "text-align: center; padding: 50px;",
                        icon("bar-chart", "fa-3x", style = "color: #ccc;"),
                        h4("Configure Parameters", style = "color: #ccc;"),
                        p("Select parameters and click 'Update Plot' to generate distribution plots", style = "color: #999;")
                      )
                    ),
                    conditionalPanel(
                      condition = sprintf("output['%s'] == true", ns("dist_plot_available")),
                      plotOutput(ns("distribution_plot"), height = "600px")
                    )
                  )
                )
              )
            ),

            # Density Analysis Tab
            tabPanel(
              title = tagList(icon("area-chart"), "Density"),
              value = "density",
              fluidRow(
                column(
                  3,
                  bs4Dash::box(
                    title = "Density Parameters",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    checkboxGroupInput(
                      ns("density_predictors"),
                      "Predictors:",
                      choices = list(
                        "Depth" = "depth",
                        "Fetch" = "fetch"
                      ),
                      selected = c("depth", "fetch")
                    ),
                    checkboxInput(
                      ns("density_post_hoc"),
                      "Use post-hoc results",
                      value = TRUE
                    ),
                    numericInput(
                      ns("density_max_depth"),
                      "Max depth (m):",
                      value = 30,
                      min = 5,
                      max = 100,
                      step = 5
                    ),
                    br(),
                    actionButton(
                      ns("update_density_plot"),
                      "Update Plot",
                      class = "btn-primary btn-block",
                      icon = icon("refresh")
                    ),
                    br(),
                    downloadButton(
                      ns("download_density_plot"),
                      "Download Plot",
                      class = "btn-success btn-block",
                      icon = icon("download")
                    )
                  )
                ),
                column(
                  9,
                  bs4Dash::box(
                    title = "Density Analysis",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    conditionalPanel(
                      condition = sprintf("output['%s'] == false", ns("density_plot_available")),
                      div(
                        style = "text-align: center; padding: 50px;",
                        icon("area-chart", "fa-3x", style = "color: #ccc;"),
                        h4("Configure Parameters", style = "color: #ccc;"),
                        p("Select parameters and click 'Update Plot' to generate density plots", style = "color: #999;")
                      )
                    ),
                    conditionalPanel(
                      condition = sprintf("output['%s'] == true", ns("density_plot_available")),
                      plotOutput(ns("density_plot"), height = "600px")
                    )
                  )
                )
              )
            ),

            # Spatial Mapping Tab
            tabPanel(
              title = tagList(icon("map"), "Spatial Map"),
              value = "spatial",
              fluidRow(
                column(
                  12,
                  bs4Dash::box(
                    title = "Interactive Spatial Map",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    fluidRow(
                      column(
                        3,
                        selectInput(
                          ns("map_layer"),
                          "Layer to Display:",
                          choices = list(),
                          width = "300px"
                        )
                      ),
                      column(4,
                        style = "margin-top: 25px;",
                        downloadButton(
                          ns("download_spatial_2"),
                          "Download Spatial Data (GPKG)",
                          class = "btn-info btn-block",
                          icon = icon("map")
                        )
                      )
                    ),
                    br(),
                    leaflet::leafletOutput(ns("spatial_map"), height = "600px")
                  )
                )
              )
            ),

            # Data Explorer Tab
            tabPanel(
              title = tagList(icon("table"), "Data Explorer"),
              value = "data_explorer",
              fluidRow(
                column(
                  3,
                  bs4Dash::box(
                    title = "Table Options",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    checkboxGroupInput(
                      ns("table_columns"),
                      "Display Columns:",
                      choices = list(),
                      selected = NULL
                    ),
                    checkboxInput(
                      ns("table_post_hoc"),
                      "Use post-hoc results",
                      value = TRUE
                    ),
                    br(),
                    downloadButton(
                      ns("download_data"),
                      "Download Data (CSV)",
                      class = "btn-success btn-block",
                      icon = icon("download")
                    ),
                    br(),
                    downloadButton(
                      ns("download_spatial"),
                      "Download Spatial Data (GPKG)",
                      class = "btn-info btn-block",
                      icon = icon("map")
                    )
                  )
                ),
                column(
                  9,
                  bs4Dash::box(
                    title = "Interactive Data Table",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    DT::DTOutput(ns("data_table"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Results Visualization Module Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param app_data Reactive values object from main app
#'
#' @noRd
#'
mod_results_viz_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for module
    values <- reactiveValues(
      dist_plot = NULL,
      density_plot = NULL,
      dist_plot_ready = FALSE,
      density_plot_ready = FALSE
    )

    # Check if results are available
    output$results_available <- reactive({
      !is.null(app_data$model_results) && app_data$model_applied
    })
    outputOptions(output, "results_available", suspendWhenHidden = FALSE)

    # Update available choices based on data
    observe({
      req(app_data$model_results)

      data <- app_data$model_results
      available_cols <- names(data)

      # Update distribution predictors
      dist_choices <- list()
      if ("depth_m" %in% available_cols) dist_choices[["Depth"]] <- "depth"
      if ("fetch_km" %in% available_cols) dist_choices[["Fetch"]] <- "fetch"

      updateCheckboxGroupInput(
        session, "dist_predictors",
        choices = dist_choices,
        selected = names(dist_choices)
      )

      updateCheckboxGroupInput(
        session, "density_predictors",
        choices = dist_choices,
        selected = names(dist_choices)
      )

      # Update map layer dropdown
      map_choices <- list()
      if ("pa_pred" %in% available_cols) {
        map_choices[["Presence/Absence Predictions"]] <- "pa_pred"
      }
      if ("pa_post_hoc" %in% available_cols) {
        map_choices[["Presence/Absence (Post-hoc)"]] <- "pa_post_hoc"
      }
      if ("cover_pred" %in% available_cols) {
        map_choices[["Cover Predictions"]] <- "cover_pred"
      }
      if ("cover_post_hoc" %in% available_cols) {
        map_choices[["Cover (Post-hoc)"]] <- "cover_post_hoc"
      }
      if ("depth_m" %in% available_cols) {
        map_choices[["Depth Values"]] <- "depth_m"
      }
      if ("fetch_km" %in% available_cols) {
        map_choices[["Fetch Values"]] <- "fetch_km"
      }

      updateSelectInput(
        session, "map_layer",
        choices = map_choices,
        selected = if (length(map_choices) > 0) map_choices[[1]] else NULL
      )

      # Update table columns
      display_cols <- available_cols[!available_cols %in% c("geometry")]
      table_choices <- stats::setNames(display_cols, display_cols)

      updateCheckboxGroupInput(
        session, "table_columns",
        choices = table_choices,
        selected = display_cols[1:min(10, length(display_cols))]
      )
    })

    # Summary outputs
    output$data_summary <- renderUI({
      req(app_data$model_results)

      data <- app_data$model_results
      summary_info <- get_data_summary(app_data)

      tagList(
        p(strong("Total points:"), summary_info$n_points),
        p(strong("Original data loaded:"), if (summary_info$original_loaded) "Yes" else "No"),
        p(strong("Fetch calculated:"), if (summary_info$fetch_calculated) "Yes" else "No"),
        p(strong("Depth extracted:"), if (summary_info$depth_extracted) "Yes" else "No"),
        p(strong("Model applied:"), if (summary_info$model_applied) "Yes" else "No"),
        if (!is.null(summary_info$model_timestamp)) {
          p(strong("Last model run:"), format(summary_info$model_timestamp, "%Y-%m-%d %H:%M"))
        }
      )
    })

    output$model_summary <- renderUI({
      req(app_data$model_results)
      req(app_data$model_params)

      results <- app_data$model_results
      params <- app_data$model_params

      summary_items <- list()

      # Model type and predictions
      summary_items[[length(summary_items) + 1]] <- p(
        strong("Model type:"), params$model_type
      )

      summary_items[[length(summary_items) + 1]] <- p(
        strong("Prediction types:"), paste(params$prediction_types, collapse = ", ")
      )

      # Results summary
      if ("pa_pred" %in% names(results)) {
        pa_mean <- mean(results$pa_pred, na.rm = TRUE)
        summary_items[[length(summary_items) + 1]] <- p(
          strong("Mean presence probability:"), paste0(round(pa_mean * 100, 1), "%")
        )
      }

      if ("cover_pred" %in% names(results)) {
        cover_mean <- mean(results$cover_pred, na.rm = TRUE)
        summary_items[[length(summary_items) + 1]] <- p(
          strong("Mean cover prediction:"), paste0(round(cover_mean, 1), "%")
        )
      }

      summary_items[[length(summary_items) + 1]] <- p(
        strong("Vmax model:"), params$vmax_model
      )

      tagList(summary_items)
    })

    output$predictors_summary <- renderUI({
      req(app_data$model_results)

      data <- app_data$model_results
      summary_info <- get_data_summary(app_data)

      available_cols <- names(data)

      # Predictors
      predictors <- c()
      if ("depth_m" %in% available_cols) predictors <- c(predictors, "Depth")
      if ("fetch_km" %in% available_cols) predictors <- c(predictors, "Fetch")

      # Model outputs
      outputs <- c()
      if ("pa_pred" %in% available_cols) outputs <- c(outputs, "Presence/Absence predictions")
      if ("cover_pred" %in% available_cols) outputs <- c(outputs, "Cover predictions")
      if ("pa_post_hoc" %in% available_cols) outputs <- c(outputs, "Post-hoc Presence/Absence")
      if ("cover_post_hoc" %in% available_cols) outputs <- c(outputs, "Post-hoc Cover")

      tagList(
        h5("Available Predictors:"),
        p(if (length(predictors) > 0) paste(predictors, collapse = ", ") else "None"),
        h5("Available Model Outputs:"),
        p(if (length(outputs) > 0) paste(outputs, collapse = ", ") else "None"),
        h5("Data Columns:"),
        p(paste(available_cols[!available_cols %in% "geometry"], collapse = ", "))
      )
    })

    # Distribution plot logic
    observeEvent(input$update_dist_plot, {
      req(app_data$model_results)
      req(input$dist_type)
      req(input$dist_predictors)

      showNotification("Generating distribution plot...", type = "message", duration = 2)

      tryCatch(
        {
          data <- app_data$model_results

          # Convert sf to data.frame for plotting
          plot_data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data

          values$dist_plot <- plot_sav_distribution(
            dat = plot_data,
            type = input$dist_type,
            predictors = input$dist_predictors,
            post_hoc = input$dist_post_hoc,
            max_depth = input$dist_max_depth,
            max_fetch = input$dist_max_fetch
          )

          values$dist_plot_ready <- TRUE
          showNotification("Distribution plot generated successfully!", type = "message", duration = 3)
        },
        error = function(e) {
          showNotification(
            paste("Error generating distribution plot:", e$message),
            type = "error",
            duration = 5
          )
          values$dist_plot_ready <- FALSE
        }
      )
    })

    output$dist_plot_available <- reactive({
      values$dist_plot_ready
    })
    outputOptions(output, "dist_plot_available", suspendWhenHidden = FALSE)

    output$distribution_plot <- renderPlot({
      req(values$dist_plot)
      values$dist_plot
    })

    # Density plot logic
    observeEvent(input$update_density_plot, {
      req(app_data$model_results)
      req(input$density_predictors)

      showNotification("Generating density plot...", type = "message", duration = 2)

      tryCatch(
        {
          data <- app_data$model_results

          # Convert sf to data.frame for plotting
          plot_data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data

          values$density_plot <- plot_sav_density(
            dat = plot_data,
            predictors = input$density_predictors,
            max_depth = input$density_max_depth,
            post_hoc = input$density_post_hoc
          )

          values$density_plot_ready <- TRUE
          showNotification("Density plot generated successfully!", type = "message", duration = 3)
        },
        error = function(e) {
          showNotification(
            paste("Error generating density plot:", e$message),
            type = "error",
            duration = 5
          )
          values$density_plot_ready <- FALSE
        }
      )
    })

    output$density_plot_available <- reactive({
      values$density_plot_ready
    })
    outputOptions(output, "density_plot_available", suspendWhenHidden = FALSE)

    output$density_plot <- renderPlot({
      req(values$density_plot)
      values$density_plot
    })

    output$spatial_map <- leaflet::renderLeaflet({
      req(app_data$model_results)
      req(input$map_layer)

      pts <- app_data$model_results |>
        sf::st_make_valid()

      # Transform if needed
      if (sf::st_crs(pts)$epsg != 4326) {
        pts <- sf::st_transform(pts, 4326)
      }

      # Get the selected color variable
      color_var <- input$map_layer

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron")

      if (!is.null(color_var) && color_var %in% names(pts)) {
        # Create color palette
        pal <- leaflet::colorNumeric(
          palette = "viridis",
          domain = pts[[color_var]]
        )

        # Determine legend title based on variable
        legend_title <- switch(color_var,
          "pa_pred" = "Presence Prob.",
          "pa_post_hoc" = "Presence (Post-hoc)",
          "cover_pred" = "Cover (%)",
          "cover_post_hoc" = "Cover (Post-hoc)",
          "depth_m" = "Depth (m)",
          "fetch_km" = "Fetch (km)",
          color_var
        )

        map <- map |>
          leaflet::addCircleMarkers(
            data = pts,
            radius = 5,
            color = ~ pal(get(color_var)),
            fillOpacity = 0.8,
            stroke = TRUE,
            weight = 1
          ) |>
          leaflet::addLegend(
            pal = pal,
            values = pts[[color_var]],
            title = legend_title,
            position = "bottomright"
          )
      } else {
        # Fallback to default colors if no valid variable selected
        map <- map |>
          leaflet::addCircleMarkers(
            data = pts,
            radius = 5,
            color = "blue",
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 1
          )
      }

      map
    })

    # Data table
    output$data_table <- DT::renderDT({
      req(app_data$model_results)

      data <- app_data$model_results

      # Convert sf to data.frame for table
      table_data <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data

      # Filter columns if specified
      if (!is.null(input$table_columns) && length(input$table_columns) > 0) {
        available_cols <- names(table_data)
        selected_cols <- input$table_columns[input$table_columns %in% available_cols]
        if (length(selected_cols) > 0) {
          table_data <- table_data[, selected_cols, drop = FALSE]
        }
      }

      # Round numeric columns
      table_data <- table_data |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))
        )

      DT::datatable(
        table_data,
        options = list(
          pageLength = 15,
          dom = "frtip",
          scrollX = TRUE
        ),
        class = "cell-border stripe",
        filter = "top"
      )
    })

    # Download handlers
    output$download_dist_plot <- downloadHandler(
      filename = function() {
        paste0("sav_distribution_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(values$dist_plot)
        ggplot2::ggsave(file, values$dist_plot, width = 12, height = 8, dpi = 300)
      }
    )

    output$download_density_plot <- downloadHandler(
      filename = function() {
        paste0("sav_density_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(values$density_plot)
        ggplot2::ggsave(file, values$density_plot, width = 12, height = 8, dpi = 300)
      }
    )

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("sav_results_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(app_data$model_results)

        data <- app_data$model_results
        export_data <- data 
        if (inherits(data, "sf")) {
          coords <- sf::st_coordinates(data) |> as.data.frame()
          export_data  <- dplyr::bind_cols(
            sf::st_drop_geometry(data),
            coords |>
              dplyr::select(c(X, Y)) |>
              dplyr::rename(longitue = X, latitude = Y)
          )
         } 

        utils::write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Download handler for spatial data (GPKG)
    output$download_spatial <- downloadHandler(
      filename = function() {
        paste0("sav_results_spatial_", Sys.Date(), ".gpkg")
      },
      content = function(file) {
        req(app_data$model_results)

        data <- app_data$model_results

        # Ensure it's an sf object
        if (!inherits(data, "sf")) {
          stop("Data is not a spatial object", call. = FALSE)
        }

        # Write as GeoPackage
        sf::st_write(data, file, driver = "GPKG", delete_dsn = TRUE)
      }
    )

    # Download handler for spatial data (GPKG)
    output$download_spatial_2 <- downloadHandler(
      filename = function() {
        paste0("sav_results_spatial_", Sys.Date(), ".gpkg")
      },
      content = function(file) {
        req(app_data$model_results)

        data <- app_data$model_results

        # Ensure it's an sf object
        if (!inherits(data, "sf")) {
          stop("Data is not a spatial object", call. = FALSE)
        }

        # Write as GeoPackage
        sf::st_write(data, file, driver = "GPKG", delete_dsn = TRUE)
      }
    )
  })
}
