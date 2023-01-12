#' UI for new comparison module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
newComparisonFunctionUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(ns("newselectionfilterfunctionbutton"), "create new function"), shiny::br(), shiny::br()

  )

}

#' Server for new comparison module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
newComparisonFunctionServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$newselectionfilterfunctionbutton, {

      shinyalert::shinyalert(html = TRUE,
                             closeOnEsc = TRUE,
                             closeOnClickOutside = TRUE,
                             showConfirmButton = FALSE,text = tagList(
                               textInput(ns("functionName"), label=NULL, placeholder="Give your function a name for future use"),
                               textAreaInput(ns("filtercomp"),
                                             label="function(panel_data, panel_selected) {", placeholder=
                                               "panel_data is the tibble corresponding to a panel that is being compared to the selected panel
                                    panel_selected is the tibble representing data from the selected panel.

                                    Your output should be a single numeric score or a tibble with one row and one column

                                    Examples:

                                    panel_data %>%
                                    left_join(panel_selected, by=c('x')) %>%
                                    mutate(prod=y.x*y.y) %>%
                                    summarize(score=sqrt(sum(prod))) %>%
                                    pull(score)

                                    or

                                  drop(sqrt(panel_data$y %*% panel_selected$y))


                                ", width="100%", height="300px"), strong("}"), shiny::br(), shiny::actionButton(ns("addfunction"), "add function to list")
                             ))

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$addfunction, {


      fn_label <- input$functionName
      pipeline_variables$filter_selection_functions <- pipeline_variables$filter_selection_functions %>% dplyr::add_row(name=fn_label, fn=input$filtercomp)

      gargoyle::trigger("compare_selectize_update")

      shiny::updateTextInput(session, "functionName", value=character())
      shiny::updateTextAreaInput(session, "filtercomp", value=character())
      shiny::updateCheckboxInput(session, "newfunction", value=FALSE)
    })


  })

}

