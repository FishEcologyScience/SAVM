# Launch the ShinyApp

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "production")
options(
  shiny.maxRequestSize = 5000 * 1024^2
)
run_app()
