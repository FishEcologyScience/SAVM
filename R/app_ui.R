#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "SAVM"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "sidebar",
          shinydashboard::menuItem("Welcome", tabName = "welcome", icon = icon("home")),
          br(),
          shinydashboard::menuItem("1. Data Input", tabName = "data_input", icon = icon("upload")),
          shinydashboard::menuItem("2. Fetch Calculation", tabName = "fetch_calc", icon = icon("wind")),
          shinydashboard::menuItem("3. Depth extraction", tabName = "depth_extr", icon = icon("water")),
          shinydashboard::menuItem("4. Model Application", tabName = "model_apply", icon = icon("brain")),
          shinydashboard::menuItem("5. Results & Visualization", tabName = "results", icon = icon("chart-line")),
          shinydashboard::menuItem("6. Export", tabName = "export", icon = icon("download")),
          br(),
          shinydashboard::menuItem("Help", tabName = "help", icon = icon("question-circle"))
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "welcome",
            fluidRow(
              shinydashboard::box(
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
                  tags$li("Export results in various formats")
                ),
                hr(),
                h5("Getting Started:"),
                p("Click on '1. Data Input' to begin uploading your data, or use the sidebar navigation to jump between steps."),
                br(),
                actionButton("start_btn", "Start Analysis", class = "btn-primary")
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "data_input",
            mod_data_input_ui("data_input_1")
          ),
          shinydashboard::tabItem(
            tabName = "fetch_calc",
            mod_fetch_calc_ui("fetch_calc_1")
          ),
          shinydashboard::tabItem(
            tabName = "depth_extr",
            mod_depth_extract_ui("depth_extract_1")
          ),
          shinydashboard::tabItem(
            tabName = "model_apply",
            mod_model_apply_ui("model_apply_1")
          ),
          shinydashboard::tabItem(
            tabName = "results",
            mod_results_viz_ui("results_viz_1")
          ),
          shinydashboard::tabItem(
            tabName = "export",
            fluidRow(
              shinydashboard::box(
                title = "Step 6: Export Results",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                h4("Export & Download"),
                p("Export your results in various formats (CSV, shapefile, etc.)."),
                p("This step will be available after analysis is completed.")
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "help",
            fluidRow(
              shinydashboard::box(
                title = "Help & Documentation",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                h4("SAVM Help"),
                h5("About SAVM"),
                p("The Submerged Aquatic Vegetation Model (SAVM) was developed by the Fish Ecology Science Lab at DFO to predict SAV presence and cover in the Laurentian Great Lakes."),
                h5("Required Data Format"),
                p("Your data should include:"),
                tags$ul(
                  tags$li(strong("Required:"), " longitude, latitude coordinates"),
                  tags$li(strong("Optional:"), " depth_m, fetch_km, secchi, substrate, limitation")
                ),
                h5("Citation"),
                p(
                  "Croft-White, M.V., Tang, R., Gardner Costa, J., Doka, S.E., and Midwood, J. D. 2022. ",
                  "Modelling submerged aquatic vegetation presence and percent cover to support the development of a freshwater fish habitat management tool. ",
                  "Can. Tech. Rep. Fish. Aquat. Sci. 3497: vi + 30 p."
                )
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
