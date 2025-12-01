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
            helpText(
              tags$span(
                style = "color: #6c757d;",
                icon("info-circle"),
                " Select which columns in your data correspond to depth and fetch."
              )
            ),
            fluidRow(
              column(
                6,
                selectInput(
                  ns("depth_column"),
                  "Depth Column:",
                  choices = NULL
                )
              ),
              column(
                6,
                selectInput(
                  ns("fetch_column"),
                  "Fetch Column:",
                  choices = NULL
                ),
              )
            ),
            br(),
            h5(strong("Post-hoc Predictor Columns")),
            helpText(
              tags$span(
                style = "color: #6c757d;",
                icon("info-circle"),
                " Select which post-hoc predictors to use for refining predictions."
              )
            ),
            shinyWidgets::prettySwitch(
              inputId = ns("use_substrate"),
              label = "Use substrate limitation",
              status = "info",
              fill = TRUE,
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_substrate")),
              selectInput(
                ns("substrate_column"),
                "Substrate Column:",
                choices = NULL
              )
            ),
            shinyWidgets::prettySwitch(
              inputId = ns("use_secchi"),
              label = "Use Secchi depth (Vmax calculation)",
              status = "info",
              fill = TRUE,
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_secchi")),
              radioButtons(
                ns("secchi_source"),
                "Secchi input type:",
                choices = c("Column" = "column", "Constant value" = "constant"),
                selected = "column"
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'column'", ns("secchi_source")),
                selectInput(
                  ns("secchi_column"),
                  "Secchi Depth Column:",
                  choices = NULL
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'constant'", ns("secchi_source")),
                numericInput(
                  ns("secchi_constant"),
                  "Secchi Depth (m):",
                  value = 2,
                  min = 0,
                  max = 50,
                  step = 0.1
                )
              ),
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
                fluidRow(
                  column(
                    6,
                    numericInput(
                      ns("vmax_intercept"),
                      "Intercept:",
                      value = 1.40,
                      min = 0,
                      max = 5,
                      step = 0.01
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      ns("vmax_slope"),
                      "Slope:",
                      value = 1.33,
                      min = 0,
                      max = 5,
                      step = 0.01
                    )
                  )
                )
              ),
            ),
            shinyWidgets::prettySwitch(
              inputId = ns("use_limitation"),
              label = "Use user-supplied limitation",
              status = "info",
              fill = TRUE,
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_limitation")),
              selectInput(
                ns("limitation_column"),
                "Limitation Column:",
                choices = NULL
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
        includeHTML(app_sys("app/www/doc/model_configuration_guide.html"))
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
        col_depth <- col_names[grepl("depth", tolower(col_names))][1L]
        updateSelectInput(
          session,
          "depth_column",
          choices = col_names,
          selected = ifelse(is.na(col_depth), col_names[1], col_depth)
        )

        col_fetch <- col_names[grepl("fetch", tolower(col_names))][1L]
        updateSelectInput(
          session,
          "fetch_column",
          choices = col_names,
          selected = ifelse(is.na(col_fetch), col_names[1], col_fetch)
        )

        # Update post-hoc column dropdowns
        updateSelectInput(
          session,
          "substrate_column",
          choices = col_names,
          selected = if ("substrate" %in% col_names) "substrate" else col_names[1]
        )

        updateSelectInput(
          session,
          "secchi_column",
          choices = col_names,
          selected = if ("secchi" %in% col_names) "secchi" else col_names[1]
        )

        updateSelectInput(
          session,
          "limitation_column",
          choices = col_names,
          selected = if ("limitation" %in% col_names) "limitation" else col_names[1]
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

          # Prepare column specifications for predictors
          depth_col <- input$depth_column
          fetch_col <- input$fetch_column

          # Prepare post-hoc column specifications if enabled
          substrate_col <- if (input$use_substrate) input$substrate_column else NULL
          secchi_col <- NULL
          if (input$use_secchi) {
            if (input$secchi_source == "constant") {
              secchi_col <- "secchi_constant"
              modeling_data[[secchi_col]] <- input$secchi_constant
            } else {
              secchi_col <- input$secchi_column
            }
          }
          limitation_col <- if (input$use_limitation) input$limitation_column else NULL
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
          depth_column = input$depth_column,
          fetch_column = input$fetch_column,
          use_substrate = input$use_substrate,
          substrate_column = if (input$use_substrate) input$substrate_column else "not used",
          use_secchi = input$use_secchi,
          secchi_column = if (input$use_secchi) {
            if (input$secchi_source == "constant") {
              paste0("constant value: ", input$secchi_constant)
            } else {
              input$secchi_column
            }
          } else {
            "not used"
          },
          use_limitation = input$use_limitation,
          limitation_column = if (input$use_limitation) input$limitation_column else "not used",
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

      if ("pa_prob" %in% names(data)) {
        pa_mean <- mean(data$pa_prob, na.rm = TRUE)
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

      # Add point ID if not present
      if (!"point_id" %in% names(pts)) {
        pts$point_id <- seq_len(nrow(pts))
      }

      # Choose color variable (prefer cover, then pa)
      color_var <- if ("cover_pred" %in% names(pts)) {
        "cover_pred"
      } else if ("pa_pred" %in% names(pts)) {
        "pa_pred"
      } else {
        NULL
      }

      # Create tooltip labels
      pts_data <- sf::st_drop_geometry(pts)
      tooltip_labels <- sprintf(
        "
        <strong>Point ID:</strong> %s<br/>
        <strong>Depth:</strong> %.2f m<br/>
        <strong>Fetch:</strong> %.2f km<br/>
        <strong>PA 'pa_pred':</strong> %d<br/>
        <strong>Cover 'cover_pred':</strong> %.1f%%
        ",
        pts_data$point_id,
        if ("depth_m" %in% names(pts_data)) pts_data$depth_m else NA,
        if ("fetch_km" %in% names(pts_data)) pts_data$fetch_km else NA,
        if ("pa_pred" %in% names(pts_data)) pts_data$pa_pred else NA,
        if ("cover_pred" %in% names(pts_data)) pts_data$cover_pred else NA
      ) |> lapply(HTML)

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
            label = tooltip_labels,
            labelOptions = leaflet::labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
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
            fillOpacity = 0.7,
            label = tooltip_labels,
            labelOptions = leaflet::labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          )
      }

      map
    })
  })
}
