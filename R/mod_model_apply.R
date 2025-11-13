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
      # Parameters Section
      column(
        4,
        bs4Dash::box(
          title = tags$span(icon("brain"), " Model Application"),
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
            fluidRow(
              column(
                8,
                h5(strong("Model Configuration"))
              ),
              column(
                4,
                div(
                  style = "text-align: right; padding-top: 5px;",
                  actionButton(
                    ns("show_model_help"),
                    label = NULL,
                    icon = icon("info-circle"),
                    class = "btn-sm btn-info",
                    style = "padding: 5px 10px;"
                  )
                )
              )
            ),
            # Model Type Selection for Presence/Absence
            selectInput(
              ns("method_pa"),
              "Presence/Absence Model:",
              choices = list(
                "Random Forest" = "rf",
                "LMM" = "lmm",
                "GAM" = "gam"
              ),
              selected = "rf"
            ),
            # Model Type Selection for Cover
            selectInput(
              ns("method_cover"),
              "Cover Model:",
              choices = list(
                "Random Forest" = "rf",
                "LMM" = "lmm",
                "GAM" = "gam"
              ),
              selected = "rf"
            ),
            # PA Threshold
            numericInput(
              ns("pa_threshold"),
              "Presence/Absence Threshold:",
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.01
            ),
            helpText(
              tags$span(
                style = "color: #6c757d;",
                icon("info-circle"),
                " Probability threshold for converting presence/absence predictions to binary values."
              )
            ),
            br(),
            h5(strong("Predictor Columns")),
            shinyWidgets::prettySwitch(
              inputId = ns("custom_predictors"),
              label = "Specify custom column names",
              status = "primary",
              fill = TRUE,
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("custom_predictors")),
              helpText(
                tags$span(
                  style = "color: #6c757d;",
                  icon("info-circle"),
                  " Select which columns in your data correspond to depth and fetch."
                )
              ),
              selectInput(
                ns("depth_column"),
                "Depth Column:",
                choices = NULL
              ),
              selectInput(
                ns("fetch_column"),
                "Fetch Column:",
                choices = NULL
              )
            ),
            br(),
            h5(strong("Post-hoc Predictor Columns")),
            shinyWidgets::prettySwitch(
              inputId = ns("custom_posthoc"),
              label = "Specify custom post-hoc column names",
              status = "info",
              fill = TRUE,
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("custom_posthoc")),
              helpText(
                tags$span(
                  style = "color: #6c757d;",
                  icon("info-circle"),
                  " Select columns for post-hoc adjustments (optional predictors)."
                )
              ),
              selectInput(
                ns("substrate_column"),
                "Substrate Column (optional):",
                choices = NULL
              ),
              selectInput(
                ns("secchi_column"),
                "Secchi Depth Column (optional):",
                choices = NULL
              ),
              selectInput(
                ns("limitation_column"),
                "Limitation Column (optional):",
                choices = NULL
              )
            ),
            br(),
            h5(strong("Post-hoc Parameters")),
            helpText(tags$span(style = "color: #6c757d;", icon("info-circle"), " Chambers and Kalff (1985) equation parameters for maximum colonization depth.")),
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
        bs4Dash::box(
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

    # Show model help modal
    observeEvent(input$show_model_help, {
      showModal(modalDialog(
        title = tags$div(
          icon("brain"),
          " SAV Model Configuration Guide"
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$div(
          style = "font-size: 14px;",
          p(
            strong("Overview:"),
            " Apply SAV prediction models using depth and fetch data. Three statistical methods are available:"
          ),
          tags$ul(
            tags$li(
              strong("Random Forest (RF):"),
              " Non-parametric ensemble method, robust to non-linear relationships. Best for complex patterns and when you don't need to interpret individual variable effects."
            ),
            tags$li(
              strong("Linear Mixed Model (LMM):"),
              " Parametric approach accounting for hierarchical data structure. Useful when you have grouped data and want to account for random effects."
            ),
            tags$li(
              strong("Generalized Additive Model (GAM):"),
              " Flexible semi-parametric method with smooth functions. Good balance between interpretability and flexibility for non-linear relationships."
            )
          ),
          hr(),
          p(
            strong("Model Selection:"),
            " You can select different methods for Presence/Absence and Cover predictions. For example, you might use Random Forest for presence/absence and GAM for cover estimation."
          ),
          p(
            strong("PA Threshold:"),
            " Adjust the probability threshold (0-1) for converting presence/absence predictions to binary values. Lower values (e.g., 0.3) are more liberal in predicting SAV presence, while higher values (e.g., 0.7) are more conservative."
          ),
          p(
            strong("Predictor Columns:"),
            " By default, the model auto-detects columns named 'depth', 'depth_m', 'fetch', or 'fetch_km'. Enable custom column specification if your data uses different column names."
          ),
          p(
            strong("Post-hoc Adjustments:"),
            " Optional columns (secchi depth, substrate, limitation) can be used to refine predictions:",
            tags$ul(
              tags$li(strong("Secchi depth:"), " Used to calculate maximum colonization depth (Vmax) based on light availability."),
              tags$li(strong("Substrate:"), " Binary indicator of substrate limitations that prevent SAV growth."),
              tags$li(strong("Limitation:"), " User-supplied limitation data for additional constraints.")
            )
          ),
          p(
            strong("Vmax Parameters:"),
            " Chambers and Kalff (1985) equation parameters control how light availability affects maximum colonization depth. Model A uses parameters from Quebec and international lakes, while Model B is calibrated for Quebec lakes only."
          )
        )
      ))
    })

    # Update column choices when data is available
    observe({
      req(app_data$original_data)

      # Get column names from the assembled modeling data
      modeling_data <- tryCatch(
        assemble_modeling_data(app_data),
        error = function(e) NULL
      )

      if (!is.null(modeling_data)) {
        # Drop geometry if sf object
        if (inherits(modeling_data, "sf")) {
          col_names <- names(sf::st_drop_geometry(modeling_data))
        } else {
          col_names <- names(modeling_data)
        }

        # Update dropdown choices
        updateSelectInput(
          session,
          "depth_column",
          choices = col_names,
          selected = if ("depth_m" %in% col_names) "depth_m" else if ("depth" %in% tolower(col_names)) col_names[which(tolower(col_names) == "depth")[1]] else col_names[1]
        )

        updateSelectInput(
          session,
          "fetch_column",
          choices = col_names,
          selected = if ("fetch_km" %in% col_names) "fetch_km" else if ("fetch" %in% tolower(col_names)) col_names[which(tolower(col_names) == "fetch")[1]] else col_names[1]
        )

        # Update post-hoc column dropdowns with "None" option
        posthoc_choices <- c("None" = "", col_names)

        updateSelectInput(
          session,
          "substrate_column",
          choices = posthoc_choices,
          selected = if ("substrate" %in% col_names) "substrate" else ""
        )

        updateSelectInput(
          session,
          "secchi_column",
          choices = posthoc_choices,
          selected = if ("secchi" %in% col_names) "secchi" else ""
        )

        updateSelectInput(
          session,
          "limitation_column",
          choices = posthoc_choices,
          selected = if ("limitation" %in% col_names) "limitation" else ""
        )
      }
    })

    # Apply models
    observeEvent(input$apply_model, {
      req(app_data$original_data)

      showNotification("Applying SAV models...", type = "message", duration = 2)

      # Run the model application safely
      shinycssloaders::showPageSpinner(
        background = "#cccccccc",
        color = "#333333",
        caption = "Applying model",
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

          # Prepare column specifications if custom predictors are enabled
          depth_col <- if (input$custom_predictors) input$depth_column else NULL
          fetch_col <- if (input$custom_predictors) input$fetch_column else NULL

          # Prepare post-hoc column specifications if enabled
          substrate_col <- if (input$custom_posthoc && nzchar(input$substrate_column)) input$substrate_column else NULL
          secchi_col <- if (input$custom_posthoc && nzchar(input$secchi_column)) input$secchi_column else NULL
          limitation_col <- if (input$custom_posthoc && nzchar(input$limitation_column)) input$limitation_column else NULL

          # Apply the model to assembled data
          sav_model(
            dat = modeling_data,
            method_pa = input$method_pa,
            method_cover = input$method_cover,
            pa_threshold = input$pa_threshold,
            depth = depth_col,
            fetch = fetch_col,
            substrate = substrate_col,
            secchi = secchi_col,
            limitation = limitation_col,
            vmax_par = vmax_par
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
          method_pa = input$method_pa,
          method_cover = input$method_cover,
          pa_threshold = input$pa_threshold,
          custom_predictors = input$custom_predictors,
          depth_column = if (input$custom_predictors) input$depth_column else "auto-detected",
          fetch_column = if (input$custom_predictors) input$fetch_column else "auto-detected",
          custom_posthoc = input$custom_posthoc,
          substrate_column = if (input$custom_posthoc && nzchar(input$substrate_column)) input$substrate_column else "auto-detected",
          secchi_column = if (input$custom_posthoc && nzchar(input$secchi_column)) input$secchi_column else "auto-detected",
          limitation_column = if (input$custom_posthoc && nzchar(input$limitation_column)) input$limitation_column else "auto-detected",
          vmax_model = input$vmax_model,
          vmax_intercept = if (input$vmax_model == "custom") input$vmax_intercept else NULL,
          vmax_slope = if (input$vmax_model == "custom") input$vmax_slope else NULL,
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

      # 
      summary_items[[length(summary_items) + 1]] <- p(
        strong("Predictors used: Fetch and Depth"),
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
  })
}
