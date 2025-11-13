#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(title = "SAVM"),
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          id = "sidebar",
          bs4Dash::menuItem("Welcome", tabName = "welcome", icon = icon("home")),
          br(),
          shiny::tagAppendAttributes(
            bs4Dash::menuItem(
              shiny::tagList(
                shiny::span("1. Data Input", class = "step-label"),
                icon("check-circle", class = "step-check", id = "check-data-input")
              ),
              tabName = "data_input",
              icon = icon("upload")
            ),
            id = "menu-data-input",
            class = "step-entry"
          ),
          shiny::tagAppendAttributes(
            bs4Dash::menuItem(
              shiny::tagList(
                shiny::span("2. Fetch Calculation", class = "step-label"),
                icon("check-circle", class = "step-check", id = "check-fetch")
              ),
              tabName = "fetch_calc",
              icon = icon("wind")
            ),
            id = "menu-fetch",
            class = "step-entry"
          ),
          shiny::tagAppendAttributes(
            bs4Dash::menuItem(
              shiny::tagList(
                shiny::span("3. Depth extraction", class = "step-label"),
                icon("check-circle", class = "step-check", id = "check-depth")
              ),
              tabName = "depth_extr",
              icon = icon("water")
            ),
            id = "menu-depth",
            class = "step-entry"
          ),
          shiny::tagAppendAttributes(
            bs4Dash::menuItem(
              shiny::tagList(
                shiny::span("4. Model Application", class = "step-label"),
                icon("check-circle", class = "step-check", id = "check-model")
              ),
              tabName = "model_apply",
              icon = icon("brain")
            ),
            id = "menu-model",
            class = "step-entry"
          ),
          br(),
          bs4Dash::menuItem("Input table", tabName = "input_tab", icon = icon("table")),
          bs4Dash::menuItem("Results & Visualization", tabName = "results", icon = icon("chart-line")),
          br(),
          bs4Dash::menuItem("Help", tabName = "help", icon = icon("question-circle"))
        )
      ),
      bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "welcome",
            fluidRow(
              bs4Dash::box(
                title = tags$span(icon("home"), " Welcome to the SAVM Shiny Application"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                p("This application provides a user-friendly interface for the SAVM (Submerged Aquatic Vegetation Model) package. The package allows users to import spatial and tabular data related to SAV presence and habitat conditions in aquatic ecosystems."),
                p("Follow the steps in the sidebar to:"),
                tags$ol(
                  tags$li("Upload and validate your spatial data for modelling"),
                  tags$li("Optionally calculate wind fetch for your points"),
                  tags$li("Optionally extract depth for your points from a bathymetry raster"),
                  tags$li("Apply Random Forest models to predict SAV presence/cover"),
                  tags$li("Visualize and explore your results"),
                ),
                hr(),
                h5("Getting Started:"),
                p("Click on '1. Data Input' to begin uploading your data, or use the sidebar navigation to jump between steps."),
                br(),
                actionButton("start_btn", "Start Analysis", class = "btn-primary")
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "data_input",
            mod_data_input_ui("data_input_1")
          ),
          bs4Dash::tabItem(
            tabName = "fetch_calc",
            mod_fetch_calc_ui("fetch_calc_1")
          ),
          bs4Dash::tabItem(
            tabName = "depth_extr",
            mod_depth_extract_ui("depth_extract_1")
          ),
          bs4Dash::tabItem(
            tabName = "model_apply",
            mod_model_apply_ui("model_apply_1")
          ),
          bs4Dash::tabItem(
            tabName = "input_tab",
            mod_input_tab_ui("input_tab_1")
          ),
          bs4Dash::tabItem(
            tabName = "results",
            mod_results_viz_ui("results_viz_1")
          ),
          bs4Dash::tabItem(
            tabName = "help",
            fluidRow(
              bs4Dash::box(
                title = "Help & Documentation",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                includeHTML(app_sys("app/www/doc/help.html"))
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "SAVM"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
