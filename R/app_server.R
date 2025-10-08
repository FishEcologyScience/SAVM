#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Reactive values to store application state
  app_data <- reactiveValues(
    # Data flow states
    data_loaded = FALSE,
    data_valid = FALSE,
    fetch_calculated = FALSE,
    model_applied = FALSE,

    # Data objects
    sav_data = NULL,
    fetch_results = NULL,
    model_results = NULL
  )

  # Navigation: Start button functionality
  observeEvent(input$start_btn, {
    shinydashboard::updateTabItems(session, "sidebar", "data_input")
  })

  # Module servers
  mod_data_input_server("data_input_1", app_data, app_session = session)
  mod_fetch_calc_server("fetch_calc_1", app_data, app_session = session)
}
