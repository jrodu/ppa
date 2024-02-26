#' UI for ecdf module
#'
#'
#' @param id id for module
#'
#'
#'
plotECDFUI <- function(id) {

  ns <- shiny::NS(id)
  shinyjs::hidden(shiny::div(id=ns("ecdf"),
  shiny::tagList(
  shiny::tags$div(id = ns("d3_output_2"), style="width: 100%;height: 300px"),
  shiny::tags$script(shiny::HTML(paste0("var filterpltdiv=d3.select('#",
                                        ns("d3_output_2"), "');"))),
  shiny::tags$script(shiny::HTML(paste0("var lclick='",
                                        ns("horizontal_line_change"), "';"))),
  shiny::includeScript(path=system.file("js", "ecdf.js", package = "ppa")),
  shiny::br(),
  shiny::conditionalPanel(
    condition = "input.has_ecdf == true",
    shiny::actionButton(ns("invertSelection"), "Invert Selection")),
  shiny::hr()
  )
)
)

}


#' Server for ecdf module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
plotECDFServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){


    observeEvent(input$horizontal_line_change, {
      cutoff <- stats::quantile(pipeline_variables$filtereddf$score,
                                probs=input$horizontal_line_change)
      if(pipeline_variables$invert_selection==0){
        filter_cutoff <- pipeline_variables$filtereddf %>%
          dplyr::filter(.data$score<cutoff)
      } else {
        filter_cutoff <- pipeline_variables$filtereddf %>%
          dplyr::filter(.data$score>cutoff)
      }
      session$sendCustomMessage(type = 'panelPlot_set', message = list(
        sel = jsonlite::toJSON(filter_cutoff$panel_string)))

    })

    observeEvent(input$invertSelection, {
      session$sendCustomMessage(type = 'invert_selection', message = "invert")
      pipeline_variables$invert_selection<-
        1-pipeline_variables$invert_selection


      prob_cut <- ifelse(is.null(input$horizontal_line_change),
                         .05, input$horizontal_line_change)

      cutoff <- stats::quantile(pipeline_variables$filtereddf$score,
                                probs=prob_cut)
      if(pipeline_variables$invert_selection==0){
        filter_cutoff <- pipeline_variables$filtereddf %>%
          dplyr::filter(
            !.data$panel_string %in% pipeline_variables$hide_panels) %>%
          dplyr::filter(.data$score<cutoff)
      } else {
        filter_cutoff <- pipeline_variables$filtereddf %>%
          dplyr::filter(
            !.data$panel_string %in% pipeline_variables$hide_panels) %>%
          dplyr::filter(.data$score>cutoff)
      }
      session$sendCustomMessage(type = 'panelPlot_set', message = list(
        sel = jsonlite::toJSON(filter_cutoff$panel_string)))
    })





  })

}

