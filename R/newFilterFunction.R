#' UI for new filter function module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
newFilterFunctionUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(ns("newvaluefilterfunctionbutton"), "create new function"), shiny::br(), shiny::br()
  )

}

#' Server for new filter function module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
newFilterFunctionServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$newvaluefilterfunctionbutton, {

      shinyalert::shinyalert(html = TRUE,
                             closeOnEsc = TRUE,
                             closeOnClickOutside = TRUE,
                             showConfirmButton = FALSE,text = tagList(
                               textInput(ns("functionPlacerName"), label=NULL, placeholder="Give your function a name for future use"),
                               textAreaInput(ns("placerval"),
                                             label="function(panel_data) {", placeholder=
                                               "panel_data is a tibble representing data from one of the panels

                       Your output should be: a single numeric score or a tibble with one row and one column

                       Examples:

                       panel_data %>% summarize(max=max(y)) %>% pull(max)

                       or

                       max(panel_data$y)


                     ", width="100%", height="300px"), strong("}"), shiny::br(), shiny::actionButton(ns("addfilterfunction"), "add function to list")
                             ))

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$addfilterfunction, {


      fn_label <- input$functionPlacerName
      pipeline_variables$filter_value_functions <- pipeline_variables$filter_value_functions %>% dplyr::add_row(name=fn_label, fn=input$placerval)


      shiny::updateTextInput(session, "functionPlacerName", value=character())
      shiny::updateTextAreaInput(session, "placerval", value=character())


      gargoyle::trigger("filter_selectize_update")

    })

  })

}

