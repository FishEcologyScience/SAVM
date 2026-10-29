mod_modal_welcome <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    query_modal <- modalDialog(
      tagList(
        includeHTML(app_sys("app/www/doc/welcome.html"))
      ),
      title = "Welcome to SAVM",
      easyClose = FALSE,
      size = "l",
      footer = tagList(
        actionButton(ns("dismiss"), "OK")
      )
    )

    observeEvent(r$show_welcome_dialog, {
      if (r$show_welcome_dialog) {
        showModal(query_modal)
      }
    })

    observeEvent(input$dismiss, {
      removeModal()
      r$show_welcome_dialog <- FALSE
    })
  })
}
