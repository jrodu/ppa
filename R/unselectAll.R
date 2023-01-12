#' UI for unselect module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
unselectAllUI <- function(id) {

  ns <- shiny::NS(id)
  shinyjs::hidden(uiOutput(ns("unselect")))



}

#' Server for unselect module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
unselectAllServer <- function(id, pipeline_variables) {


  shiny::moduleServer(id, function(input, output, session){


    ns <- session$ns

    output$unselect <- shiny::renderUI(
      shiny::tagList(shiny::tags$h4("Panel Selection Actions"),
                     shiny::tags$p("'Unselect All' de-selects all actively selected panels.
                                   'Examine Selected' isolates and zooms in on the selected panels.
                                   'Return to all panels' reverses the effect of 'Examine Selected'."),
                     shiny::actionButton(ns("selectNone"), "Unselect All"),
                     shiny::actionButton(ns("isolateSelected"), "Examine Selected"),
                     shiny::actionButton(ns("unSelect"), "Return to all panels"),
                     #shiny::checkboxInput(ns("slctrow"), "select by row"),
                     #shiny::checkboxInput(ns("slctcol"), "select by col")
                     shiny::hr()
                     )
    )


    shiny::observeEvent(input$selectNone, {
      session$sendCustomMessage(type = 'panelPlot_set', message = list(sel=character(0)))
      pipeline_variables$selected_state <- NULL
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$isolateSelected, {
      if(!is.null(pipeline_variables$selected_state)){
      pipeline_variables$isolate_panels <- setdiff(pipeline_variables$df_main$panel_string, pipeline_variables$selected_state)
      pipeline_variables$shift_data()
      pipeline_variables$trigger_tmpnewdat()
      gargoyle::trigger("send_to_panel_plot")
      }
    }, ignoreInit = TRUE)


    shiny::observeEvent(input$unSelect, {
      pipeline_variables$isolate_panels <- NULL
      pipeline_variables$shift_data()
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)

    # shiny::observeEvent(input$slctrow, {
    #   pipeline_variables$select_by_row <- input$slctrow
    # }, ignoreInit = TRUE)
    #
    # shiny::observeEvent(input$slctcol, {
    #   pipeline_variables$select_by_col <- input$slctrow
    # }, ignoreInit = TRUE)

  })


}

