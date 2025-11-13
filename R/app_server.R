#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Reactive values to store application state
  app_data <- reactiveValues(
    #
    show_welcome_dialog = get_golem_config("show_welcome_dialog"),
    # Data flow states
    data_loaded = FALSE,
    data_valid = FALSE,
    fetch_calculated = FALSE,
    depth_extracted = FALSE,
    model_applied = FALSE,

    # Original data (never modified)
    original_data = NULL,

    # Individual calculation results (stored separately)
    fetch_results = NULL,
    depth_results = NULL,
    model_results = NULL,

    # Calculation metadata
    fetch_params = NULL,
    depth_params = NULL,
    model_params = NULL,

    # Calculation timestamps
    fetch_timestamp = NULL,
    depth_timestamp = NULL,
    model_timestamp = NULL
  )

  # Navigation: Start button functionality
  observeEvent(input$start_btn, {
    bs4Dash::updateTabItems(session, "sidebar", "data_input")
  })

  # Module servers
  mod_modal_welcome("welcome", app_data)
  mod_data_input_server("data_input_1", app_data, app_session = session)
  mod_fetch_calc_server("fetch_calc_1", app_data, app_session = session)
  mod_depth_extract_server("depth_extract_1", app_data, app_session = session)
  mod_model_apply_server("model_apply_1", app_data, app_session = session)
  mod_results_viz_server("results_viz_1", app_data)
  mod_input_tab_server("input_tab_1", app_data)

  observeEvent(app_data$data_loaded, {
    shinyjs::toggleClass(
      id = "check-data-input",
      class = "is-visible",
      condition = isTRUE(app_data$data_loaded)
    )
  }, ignoreNULL = FALSE)

  observeEvent(app_data$fetch_calculated, {
    shinyjs::toggleClass(
      id = "check-fetch",
      class = "is-visible",
      condition = isTRUE(app_data$fetch_calculated)
    )
  }, ignoreNULL = FALSE)

  observeEvent(app_data$depth_extracted, {
    shinyjs::toggleClass(
      id = "check-depth",
      class = "is-visible",
      condition = isTRUE(app_data$depth_extracted)
    )
  }, ignoreNULL = FALSE)

  observeEvent(app_data$model_applied, {
    shinyjs::toggleClass(
      id = "check-model",
      class = "is-visible",
      condition = isTRUE(app_data$model_applied)
    )
  }, ignoreNULL = FALSE)
}
