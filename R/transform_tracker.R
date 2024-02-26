#' UI for transform tracker module
#'
#'
#' @param id id for module
#'
#'
#'
transformTrackerUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(shiny::br(), shiny::br(),
                 shiny::p("Click on work flow point to return"),
                 shiny::tags$div(id = ns("d3_output_3"),
                                 style="width: 100%;height: 300px"),
                 shiny::tags$script(
                   shiny::HTML(
                     paste0("var treepltdiv=d3.select('#", ns("d3_output_3"),
                            "');"))),
                 shiny::tags$script(shiny::HTML(
                   paste0("var stanch='", ns("set_anchor"), "';"))),
                 shiny::includeScript(
                   path=system.file("js", "working_path.js", package = "ppa")))

}

#' Server for transform tracker module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
transformTrackerServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    observeEvent(input$set_anchor, {
      pipeline_variables$current_anchor <- as.double(input$set_anchor)
      gargoyle::trigger("filter_chain")
    }, ignoreInit = TRUE)


  })
}

