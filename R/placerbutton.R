#' UI for placement button module
#'
#'
#' @param id id for module
#' @param button name to appear in text of button
#'
#' @importFrom magrittr %>%
#'
#'
placerbuttonUI <- function(id, button) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectizeInput(ns("placebyvalue"), label=NULL, choices=character(), selected = NULL, multiple = FALSE,
                          options = list(placeholder=paste("select", button, "function")))
  )

}

#' Server for placement button module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
placerbuttonServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    shiny::observe({

      gargoyle::watch("filter_selectize_update")
      shiny::updateSelectizeInput(session, inputId = "placebyvalue", choices=pipeline_variables$filter_value_functions$name, selected=character())

    })

    return(reactive({input$placebyvalue}))


  })
}
