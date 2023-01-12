#' UI for examine module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
examineUI <- function(id) {

  ns <- shiny::NS(id)

  uiOutput(ns("examine"))
  # shiny::tagList(shiny::tags$h4("Examine panels more closely"),
  #                shiny::tags$p("Select panels and click 'Examine selected' to
  #                              isolate and study them.  Click 'Return to all panels'
  #                              to go back"),
  #   shiny::actionButton(ns("isolateSelected"), "Examine Selected"),
  #   shiny::actionButton(ns("unSelect"), "Return to all panels")
  # )

}

#' Server for examine module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
examineServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$examine <- shiny::renderUI(

      shiny::tagList(shiny::tags$h4("Examine panels more closely"),
                     shiny::tags$p("Select panels and click 'Examine selected' to
                               isolate and study them.  Click 'Return to all panels'
                               to go back"),
                     shiny::actionButton(ns("isolateSelected"), "Examine Selected"),
                     shiny::actionButton(ns("unSelect"), "Return to all panels"),
                     shiny::hr()
      )

    )


    shiny::observeEvent(input$isolateSelected, {
      pipeline_variables$isolate_panels <- setdiff(pipeline_variables$df_main$panel_string, pipeline_variables$selected_state)
      pipeline_variables$shift_data()
      pipeline_variables$trigger_tmpnewdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)


    shiny::observeEvent(input$unSelect, {
      pipeline_variables$isolate_panels <- NULL
      pipeline_variables$shift_data()
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)


  })

}

