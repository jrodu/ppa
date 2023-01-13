#' UI for comparison button module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
comparisonbuttonUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectizeInput(ns("comparisonbyvalue"), label=NULL, choices=character(), selected = NULL, multiple = FALSE,
                          options = list(placeholder="select comparison function"))
  )

}

#' Server for comparison button module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
comparisonbuttonServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    shiny::observe({

      gargoyle::watch("compare_selectize_update")
      shiny::updateSelectizeInput(session, inputId = "comparisonbyvalue", choices=pipeline_variables$filter_selection_functions$name, selected=character())

    })

    return(reactive({input$comparisonbyvalue}))


  })
}
