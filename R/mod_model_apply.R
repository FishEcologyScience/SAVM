#' Model Application Module UI Function
#'
#' @description A shiny Module for applying SAV models
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_model_apply_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                12,
                shinydashboard::box(
                    title = tags$span(icon("brain"), " Model Application"),
                    collapsible = TRUE,
                    collapsed = TRUE,
                    status = "primary",
                    width = NULL,
                    solidHeader = TRUE,
                    p("In this section, you can apply Random Forest models to predict SAV presence and cover based on your data."),
                    p("The models use depth and/or fetch as predictors, with optional post-hoc processing using substrate, secchi depth, and user-defined limitations."),
                    p("Select your model type and parameters below to generate predictions for your sampling points.")
                )
            )
        ),
        fluidRow(
            # Parameters Section
            column(
                4,
                shinydashboard::box(
                    title = "Model Parameters",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    conditionalPanel(
                        condition = sprintf("output['%s'] == false", ns("data_available")),
                        div(
                            style = "text-align: center; padding: 20px;",
                            icon("exclamation-triangle", "fa-2x", style = "color: #f39c12;"),
                            h4("No Data Available", style = "color: #f39c12;"),
                            p("Please complete the Data Input step first.", style = "color: #7f8c8d;")
                        )
                    ),
                    conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("data_available")),
                        h4("Model Configuration"),

                        # Model Type Selection (future enhancement)
                        selectInput(
                            ns("model_type"),
                            "Model Type:",
                            choices = list(
                                "Random Forest" = "rf",
                                "GLM" = "glm",
                                "GAMM" = "gamm"
                            ),
                            selected = "rf"
                        ),
                        conditionalPanel(
                            condition = sprintf("input['%s'] != 'rf'", ns("model_type")),
                            div(
                                style = "background-color: #fff3cd; border: 1px solid #ffeaa7;
                                         border-radius: 4px; padding: 10px; margin: 10px 0;",
                                div(
                                    style = "color: #856404;",
                                    icon("info-circle", style = "margin-right: 8px;"),
                                    strong("Coming Soon:")
                                ),
                                p(
                                    style = "margin: 5px 0 0 0; color: #856404;",
                                    "GLM and GAMM models are not yet implemented. Currently only Random Forest is available."
                                )
                            )
                        ),

                        # Prediction Type
                        checkboxGroupInput(
                            ns("prediction_type"),
                            "Prediction Type:",
                            choices = list(
                                "Presence/Absence" = "pa",
                                "Cover" = "cover"
                            ),
                            selected = c("pa", "cover")
                        ),
                        h4("Post-hoc Parameters"),
                        helpText(tags$span(icon("info-circle"), " Chambers and Kalff (1985) equation parameters for maximum colonization depth.")),
                        selectInput(
                            ns("vmax_model"),
                            "Vmax Model:",
                            choices = list(
                                "Model A (Quebec + International lakes)" = "model_a",
                                "Model B (Quebec lakes only)" = "model_b",
                                "Custom" = "custom"
                            ),
                            selected = "model_a"
                        ),
                        conditionalPanel(
                            condition = sprintf("input['%s'] == 'custom'", ns("vmax_model")),
                            numericInput(
                                ns("vmax_intercept"),
                                "Intercept:",
                                value = 1.40,
                                min = 0,
                                max = 5,
                                step = 0.01
                            ),
                            numericInput(
                                ns("vmax_slope"),
                                "Slope:",
                                value = 1.33,
                                min = 0,
                                max = 5,
                                step = 0.01
                            )
                        ),
                        br(),
                        fluidRow(
                            column(2),
                            column(
                                4,
                                actionButton(
                                    ns("apply_model"),
                                    "Apply Models",
                                    class = "btn-primary btn-block",
                                    icon = icon("brain")
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
                    htmlOutput(ns("model_status")),
                    br(),
                    conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("model_complete")),
                        actionButton(
                            ns("proceed_to_results"),
                            "Proceed to Results & Visualization",
                            class = "btn-success",
                            icon = icon("chart-line")
                        )
                    )
                ),
                shinydashboard::box(
                    title = "Model Results",
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    conditionalPanel(
                        condition = sprintf("output['%s'] == false", ns("model_complete")),
                        div(
                            style = "text-align: center; padding: 50px;",
                            icon("brain", "fa-3x", style = "color: #ccc;"),
                            h4("No Models Applied", style = "color: #ccc;"),
                            p("Configure parameters and click 'Apply Models' to see results", style = "color: #999;")
                        )
                    ),
                    conditionalPanel(
                        condition = sprintf("output['%s'] == true", ns("model_complete")),
                        div(
                            fluidRow(
                                column(
                                    6,
                                    h4("Model Summary"),
                                    htmlOutput(ns("model_summary"))
                                ),
                                column(
                                    6,
                                    h4("Prediction Visualization"),
                                    leaflet::leafletOutput(ns("model_map"), height = "400px")
                                )
                            ),
                            hr(),
                            h4("Model Results Table"),
                            DT::DTOutput(ns("model_table"))
                        )
                    )
                )
            )
        )
    )
}

#' Model Application Module Server Function
#'
#' @param id Internal parameter for {shiny}.
#' @param app_data Reactive values object from main app
#'
#' @noRd
#'
mod_model_apply_server <- function(id, app_data, app_session) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Reactive values for module
        values <- reactiveValues(
            model_results = NULL,
            model_complete = FALSE
        )

        # Check if data is available
        output$data_available <- reactive({
            !is.null(app_data$original_data) && app_data$data_valid
        })
        outputOptions(output, "data_available", suspendWhenHidden = FALSE)

        # Apply models
        observeEvent(input$apply_model, {
            req(app_data$original_data)
            req(input$prediction_type)

            # Only allow Random Forest for now
            if (input$model_type != "rf") {
                showNotification(
                    "Only Random Forest models are currently available.",
                    type = "warning",
                    duration = 3
                )
                return()
            }

            showNotification("Applying SAV models...", type = "message", duration = 2)

            # Run the model application safely
            shinycssloaders::showPageSpinner(
                background = "#cccccccc",
                color = "#333333",
                caption = "Calculating Fetch",
                image = "www/img/insil.gif",
                image.width = "200",
                image.height = "200"
            )


            result <- tryCatch(
                {
                    # Assemble data for modeling (combines original + fetch + depth)
                    modeling_data <- assemble_modeling_data(app_data)
                    
                    # Prepare vmax parameters
                    vmax_par <- switch(input$vmax_model,
                        "model_a" = list(intercept = 1.40, slope = 1.33),
                        "model_b" = list(intercept = 1.32, slope = 1.14),
                        "custom" = list(intercept = input$vmax_intercept, slope = input$vmax_slope)
                    )

                    # Apply the model to assembled data
                    sav_model(
                        dat = modeling_data,
                        type = input$prediction_type,
                        vmax_par = vmax_par
                        # Note: model parameter will be added here when implemented
                        # model = input$model_type
                    )
                },
                error = function(e) {
                    showNotification(
                        paste("Error applying models:", e$message),
                        type = "error",
                        duration = 5
                    )
                    return(NULL)
                }
            )

            shinycssloaders::hidePageSpinner()

            if (!is.null(result)) {
                # Store model results separately
                values$model_results <- result
                values$model_complete <- TRUE
                
                # Update app data with model results and metadata
                app_data$model_results <- result
                app_data$model_applied <- TRUE
                app_data$model_timestamp <- Sys.time()
                
                # Store model parameters for reference
                app_data$model_params <- list(
                    model_type = input$model_type,
                    prediction_types = input$prediction_type,
                    vmax_model = input$vmax_model,
                    vmax_intercept = if (input$vmax_model == "custom") input$vmax_intercept else NULL,
                    vmax_slope = if (input$vmax_model == "custom") input$vmax_slope else NULL,
                    available_predictors = get_available_predictors(app_data),
                    n_points_modeled = nrow(result)
                )

                showNotification("Model application completed successfully!", type = "message", duration = 3)
            }
        })

        # Clear results
        observeEvent(input$clear_results, {
            # Clear model-specific results and metadata
            values$model_results <- NULL
            values$model_complete <- FALSE
            clear_calculation_results(app_data, "model")
            
            showNotification("Model results cleared.", type = "message", duration = 2)
        })

        # Output: Model complete flag
        output$model_complete <- reactive({
            values$model_complete
        })
        outputOptions(output, "model_complete", suspendWhenHidden = FALSE)

        # Output: Model summary
        output$model_summary <- renderUI({
            req(values$model_results)

            data <- values$model_results

            # Determine which predictions were made
            pred_cols <- grep("_pred$", names(data), value = TRUE)

            summary_items <- list()
            summary_items[[length(summary_items) + 1]] <- p(strong("Points processed:"), nrow(data))

            if ("pa_pred" %in% names(data)) {
                pa_mean <- mean(data$pa_pred, na.rm = TRUE)
                summary_items[[length(summary_items) + 1]] <- p(
                    strong("Mean presence probability:"),
                    paste0(round(pa_mean * 100, 1), "%")
                )
            }

            if ("cover_pred" %in% names(data)) {
                cover_mean <- mean(data$cover_pred, na.rm = TRUE)
                summary_items[[length(summary_items) + 1]] <- p(
                    strong("Mean cover prediction:"),
                    paste0(round(cover_mean, 2), "%")
                )
            }

            # Check available predictors
            predictors <- c()
            if ("depth_m" %in% names(data)) predictors <- c(predictors, "depth")
            if ("fetch_km" %in% names(data)) predictors <- c(predictors, "fetch")

            summary_items[[length(summary_items) + 1]] <- p(
                strong("Predictors used:"),
                if (length(predictors) > 0) paste(predictors, collapse = " + ") else "none detected"
            )

            tagList(summary_items)
        })

        # Output: Model results table
        output$model_table <- DT::renderDT({
            req(values$model_results)

            # Convert sf to regular data frame for preview
            model_data <- sf::st_drop_geometry(values$model_results) |>
                dplyr::mutate(
                    dplyr::across(dplyr::ends_with("_pred"), ~ round(.x, 3)),
                    dplyr::across(dplyr::ends_with("_post_hoc"), ~ round(.x, 3))
                )

            DT::datatable(
                model_data,
                options = list(
                    pageLength = 10,
                    dom = "ftip",
                    scrollX = TRUE
                ),
                class = "cell-border stripe"
            )
        })

        # Output: Model map
        output$model_map <- leaflet::renderLeaflet({
            req(values$model_results)

            pts <- values$model_results |>
                sf::st_make_valid()

            # Transform if needed
            if (sf::st_crs(pts)$epsg != 4326) {
                pts <- sf::st_transform(pts, 4326)
            }

            # Choose color variable (prefer cover, then pa)
            color_var <- if ("cover_pred" %in% names(pts)) {
                "cover_pred"
            } else if ("pa_pred" %in% names(pts)) {
                "pa_pred"
            } else {
                NULL
            }

            map <- leaflet::leaflet() |>
                leaflet::addProviderTiles("CartoDB.Positron")

            if (!is.null(color_var)) {
                # Create color palette
                pal <- leaflet::colorNumeric(
                    palette = "viridis",
                    domain = pts[[color_var]]
                )

                map <- map |>
                    leaflet::addCircleMarkers(
                        data = pts,
                        radius = 5,
                        color = ~ pal(get(color_var)),
                        fillOpacity = 0.8,
                        stroke = TRUE,
                        weight = 1,
                    ) |>
                    leaflet::addLegend(
                        pal = pal,
                        values = pts[[color_var]],
                        title = if (color_var == "cover_pred") "Cover (%)" else "Presence Prob.",
                        position = "bottomright"
                    )
            } else {
                map <- map |>
                    leaflet::addCircleMarkers(
                        data = pts,
                        radius = 5,
                        color = "blue",
                        fillOpacity = 0.7
                    )
            }

            map
        })

        # Output: Model status
        output$model_status <- renderUI({
            if (is.null(app_data$original_data) || !app_data$data_valid) {
                return(p("Please complete data input before applying models."))
            }

            if (values$model_complete) {
                return(
                    tagList(
                        div(
                            style = "color: green;",
                            icon("check-circle", "fa-2x"),
                            h4("Models Applied!", style = "display: inline; margin-left: 10px;")
                        ),
                        p("SAV models have been successfully applied to your data.")
                    )
                )
            }

            # Default state
            p("Configure model parameters and click 'Apply Models' to begin.")
        })

        # Navigation: Proceed to results and visualization
        observeEvent(input$proceed_to_results, {
            shinydashboard::updateTabItems(session = app_session, inputId = "sidebar", "results")
        })
    })
}
