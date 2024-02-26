#' UI for shuffle panel module
#'
#'
#' @param id id for module
#'
#'
#'
shufflePanelUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("shuffle")))


}

#' Server for shuffle panel module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
shufflePanelServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$shuffle <- shiny::renderUI(
      #selector = paste0("#", ns("putaxes")),
      #where = "beforeBegin",
      shiny::tagList(
        shiny::tags$h4("Shuffle panels"),
        shiny::tags$p(
          "Select the appropriate button to randomize
          the order of the panels."),
        shiny::actionButton(ns("row"), "Shuffle Rows"),
        shiny::actionButton(ns("col"), "Shuffle Columns"),
        shiny::actionButton(ns("both"), "Shuffle Rows and Columns"),
        shiny::actionButton(ns("shufflepanels"), "Shuffle by panel"),
        shiny::hr()
      )
    )

    shiny::observeEvent(input$row, {
      pipeline_variables$force_grid <- TRUE
      pipeline_variables$setup_grid <- shuffle_row(
        pipeline_variables$setup_grid)
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$col, {
      pipeline_variables$force_grid <- TRUE
      pipeline_variables$setup_grid <- shuffle_col(
        pipeline_variables$setup_grid)
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$both, {
      pipeline_variables$force_grid <- TRUE
      pipeline_variables$setup_grid <- shuffle_rowcol(
        pipeline_variables$setup_grid)
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$shufflepanels, {
      pipeline_variables$force_grid <- TRUE
      pipeline_variables$setup_grid <- shuffle_panel(
        pipeline_variables$setup_grid)
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)
  })

}

