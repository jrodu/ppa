#' UI for exclude module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
excludeUI <- function(id) {
  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("exclude")))

  # shiny::tagList(
  #   #shiny::tags$h4("Exclude panels"),
  #   #shiny::tags$p("Select the panels you wish to exclude and click 'exclude'.
  #                 # When you are satisfied with your list, click 'Save exclusions' to
  #                 # continue working or 'Save exclusions and quit' to exit the app.
  #                 # The exclusions will be saved as a tibble in your
  #                 # global environment."),
  # shiny::actionButton(ns("hide"), "exclude"),
  # shiny::actionButton(ns("unhide"), "Unexclude all"),
  # shiny::actionButton(ns("save"), "Save exclusions"),
  # shiny::actionButton(ns("save_and_quit"), "Save exclusions and quit")
  # )

}

#' Server for exclude module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
excludeServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    output$exclude <- shiny::renderUI(
      #selector = paste0("#", ns("putaxes")),
      #where = "beforeBegin",
      shiny::tagList(
        shiny::tags$h4("Exclude panels"),
        shiny::tags$p("Select the panels you wish to exclude and click 'exclude'.
                      Note that 'Unexclude all' will unexclude all panels that
                      were excluded in this session.  Therefore if you are excluding
                      panels in waves, it might make sense to label a group of panels
                      prior to excluding them, making it easier to 're-exclude' panels later."),
        shiny::actionButton(ns("hide"), "exclude"),
        shiny::actionButton(ns("unhide"), "Unexclude all"),
        # shiny::actionButton(ns("save"), "Save exclusions"),
        # shiny::actionButton(ns("save_and_quit"), "Save exclusions and quit")
        shiny::hr()
      )
    )


    shiny::observeEvent(input$hide, {
      #first record the action:
      #here's how to do it: eval(parse(text=paste0("c(", toString(c(1,2,3,4,5)), ")")))

      pipeline_variables$hide_panels <- union(pipeline_variables$selected_state, pipeline_variables$hide_panels)

      pipeline_variables$shift_data()
      pipeline_variables$trigger_newdat()
      #update the used dataset
      gargoyle::trigger("send_to_panel_plot")
      session$sendCustomMessage(type = 'panelPlot_set', message = list(sel=character(0)))
      pipeline_variables$selected_state <- NULL
    }, ignoreInit = TRUE)


    observeEvent(input$unhide, {
      pipeline_variables$selected_state <- pipeline_variables$hide_panels

      pipeline_variables$hide_panels <- NULL
      pipeline_variables$shift_data()
      pipeline_variables$trigger_newdat()
      #update the used dataset
      gargoyle::trigger("send_to_panel_plot")
      session$sendCustomMessage(type = 'panelPlot_set', message = list(sel=jsonlite::toJSON(pipeline_variables$selected_state)))
    }, ignoreInit = TRUE)

    # shiny::observeEvent(input$save | input$save_and_quit, {
    #   tmp <-  pipeline_variables$backup_df %>% dplyr::select(panel_string) %>% unique() %>%
    #     dplyr::mutate(excludeIndicator = ifelse(panel_string %in% pipeline_variables$hide_panels, 1, 0))
    #   exclude <<- tmp
    # }, ignoreInit = TRUE)
    # shiny::observeEvent(input$save_and_quit, {
    #   shiny::stopApp()
    # }, ignoreInit = TRUE)


    })

  }

