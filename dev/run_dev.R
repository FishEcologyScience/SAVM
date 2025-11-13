golem::detach_all_attached()
golem::document_and_reload()
options(
  golem.app.prod = FALSE,
  shiny.port = httpuv::randomPort()
)
run_app()
