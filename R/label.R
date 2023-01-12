#' UI for label module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
labelUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("label")))
  # shiny::tagList(
  #   shiny::tags$h4("Label panels"),
  #   shiny::tags$p("To label selected panels, choose label from list, or create a
  #                 new label (you must click on the label to create it).  Then, click
  #                 the 'Apply Label' button.  Click 'Save labels' to save labels and
  #                 continue working or 'Save labels and quit' to exit the app.
  #                 The lables will be saved as a tibble in your
  #                 global environment."),
  #   shiny::selectizeInput(ns('label'), 'label', choices=character(),
  #                  options = list(create = TRUE, placeholder="Select labels"),
  #                  multiple=TRUE), shiny::br(),
  #   shiny::actionButton(ns("applylabel"), "Apply Label"), shiny::br(),
  #   shiny::tags$p("You can select panels by label:"),
  #   shiny::selectizeInput(ns("filterbylabel"), "Select by Label", choices=character(), selected = NULL, multiple = TRUE,
  #                  options = list(placeholder="Choose labels to select")),
  #   shiny::tagList(shiny::tags$p("Saving labels")),
  #   shiny::actionButton(ns("save"), "Save labels"),
  #   shiny::actionButton(ns("save_and_quit"), "Save labels and quit")
  # )

}

#' Server for label button module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
labelServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$label <- shiny::renderUI(
    shiny::tagList(
      shiny::tags$h4("Label panels"),
      shiny::tags$p("To label selected panels, choose label from list, or create a
                  new label by typing in the selection box.  Then, click
                  the 'Apply Label' button.
                    To select by label, select the label(s) of interest from the 'Select by Label' dropdown menu."),
      shiny::selectizeInput(ns('label'), 'Label', choices=character(),
                            options = list(create = TRUE, placeholder="Select labels"),
                            multiple=TRUE), shiny::br(),
      shiny::actionButton(ns("applylabel"), "Apply Label"), shiny::br(),
      shiny::tags$p("You can select panels by label:"),
      shiny::selectizeInput(ns("filterbylabel"), "Select by Label", choices=character(), selected = NULL, multiple = TRUE,
                            options = list(placeholder="Choose labels to select")),
      # shiny::tagList(shiny::tags$p("Saving labels")),
      # shiny::actionButton(ns("save"), "Save labels"),
      # shiny::actionButton(ns("save_and_quit"), "Save labels and quit")
      shiny::hr()
    )
    )

    shiny::observeEvent(input$applylabel, {
      # get unselected states (but not hidden) and make them 0, then when left_join the hidden ones will be NA.
      if(isTruthy(input$label)){
        pipeline_variables$update_labels(input$label)
        #print(pipeline_variables$labels %>% filter(panel_string=="1"))
        #print(pipeline_variables$labels)
        shiny::updateSelectizeInput(session, "label", selected = character())
        tmp_names <- names(pipeline_variables$labels)
        shiny::updateSelectizeInput(session, "filterbylabel", selected=character(),
                             choices=tmp_names[tmp_names!="panel_string"])
      } else {
        output$messages <- renderText("Must select a label")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$filterbylabel, {
      totselect <- NULL
      # get unselected states (but not hidden) and make them 0, then when left_join the hidden ones will be NA.
      select_this <- pipeline_variables$labels %>%
        dplyr::mutate(totselect = rowSums(dplyr::across(tidyselect::all_of(input$filterbylabel)), na.rm=T)) %>%
        dplyr::filter(totselect>0) %>% dplyr::select(.data$panel_string)
      session$sendCustomMessage(type = 'panelPlot_set', message = list(
        sel = jsonlite::toJSON(select_this$panel_string)))

    }, ignoreInit = TRUE)

    # shiny::observeEvent(input$save | input$save_and_quit, {
    #   label <<- pipeline_variables$labels
    # }, ignoreInit = TRUE)
    # shiny::observeEvent(input$save_and_quit, {
    #   shiny::stopApp()
    # }, ignoreInit = TRUE)

  })

}

