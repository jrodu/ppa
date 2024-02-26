#' UI for filter button module
#'
#'
#' @param id id for module
#'
#'
#'
filterbuttonUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectizeInput(ns("filterbyvalue"), label=NULL, choices=character(),
                          selected = NULL, multiple = FALSE,
                          options = list(placeholder="select filter function"))
  )

}

#' Server for filter button module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
filterbuttonServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    shiny::observe({

      gargoyle::watch("filter_selectize_update")
      shiny::updateSelectizeInput(session, inputId = "filterbyvalue",
                        choices=pipeline_variables$filter_value_functions$name,
                        selected=character())

    })

    return(reactive({input$filterbyvalue}))


  })
}

