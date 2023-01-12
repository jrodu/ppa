#' UI for new transform function module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
newTransformFunctionUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(ns("newtransformfunctionbutton"), "create new transform"),
    shiny::br(), shiny::br(),
  )

}

#' Server for new transform function module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
newTransformFunctionServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$newtransformfunctionbutton, {

      shinyalert::shinyalert(html = TRUE,
                 closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE,
                 showConfirmButton = FALSE,text = tagList(
                   textInput(ns("functionTransformName"), label=NULL, placeholder="Give your function a name for future use"),
                   textAreaInput(ns("transformval"),
                                 label="function(panel_data) {", placeholder=
                                   "panel_data is a tibble representing data from one of the panels

  Your output should be a transformed tibble.  Your original plotting variables
  should be transformed in order to visualize the results of the transformation.
  In other words, if your original plotted variables are x and y, after transformation
  the variables being plotted will still be x and y.

  Examples:

  panel_data %>% mutate(y=y^2)


", width="100%", height="300px"), strong("}"), br(), actionButton(ns("addtransformfunction"), "add function to list")
                 ))

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$addtransformfunction, {


      fn_label <- input$functionTransformName
      pipeline_variables$transform_functions <- pipeline_variables$transform_functions %>% dplyr::add_row(name=fn_label, fn=input$transformval)


      shiny::updateTextInput(session, "functionTransformName", value=character())
      shiny::updateTextAreaInput(session, "transformval", value=character())


      gargoyle::trigger("transform_selectize_update")

    })

  })

}

