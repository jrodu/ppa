#' UI for transform button module
#'
#'
#' @param id id for module
#'
#'
#'
transformbuttonUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectizeInput(ns("transformbyfunction"),
                          label=NULL, choices=character(), selected = NULL,
                          multiple = FALSE,
                          options = list(
                            placeholder="select transform function"))
  )

}

#' Server for transform button module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
transformbuttonServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){


    shiny::observe({

      gargoyle::watch("transform_selectize_update")
      shiny::updateSelectizeInput(
        session, inputId = "transformbyfunction",
        choices=pipeline_variables$transform_functions$name,
        selected=character())


    })

    return(reactive({input$transformbyfunction}))


  })
}
