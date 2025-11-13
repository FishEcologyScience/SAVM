# Launch the ShinyApp
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "production")
run_app()
