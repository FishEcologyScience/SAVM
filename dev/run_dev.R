golem::detach_all_attached()
golem::document_and_reload()
options(
  golem.app.prod = FALSE,
  shiny.port = httpuv::randomPort(),
  shiny.maxRequestSize = 5000 * 1024^2
)
run_app()
