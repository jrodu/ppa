#' UI for axes module
#' @importFrom magrittr %>%
#'
#' @param id module id
#'
#'
axesUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("axes")))


}


#' Server for axes module
#'
#' @param id module id
#' @param pipeline_variables R6 object containing important common variables
#'
axesServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    varnames <- names(pipeline_variables$df_main)

    output$axes <- shiny::renderUI(
          #selector = paste0("#", ns("putaxes")),
          #where = "beforeBegin",
          shiny::tagList(
            shinyjs::disabled(shiny::selectInput(ns("xvalueselect"), "X Variable", choices=names(pipeline_variables$df_main), selected = pipeline_variables$var1, multiple = FALSE)),
            shinyjs::disabled(shiny::selectInput(ns("yvalueselect"), "Y Variable", choices=names(pipeline_variables$df_main), selected = pipeline_variables$var2, multiple = FALSE)),
            shiny::tags$p(paste0("All variables in data frame: ", base::toString(varnames))),
            shiny::selectInput(ns("plottype"), "Plot Type", choices=c("scatter", "line"), selected =pipeline_variables$plot_type, multiple = FALSE),

            shiny::actionButton(ns("updatevariables"), "update plot type"),
            shiny::hr()
            )
          )

    shiny::observe({
      gargoyle::watch("update_variables")

      shiny::updateSelectizeInput(
        session,
        "xvalueselect",
        choices = names(pipeline_variables$df_main)
      )

      shiny::updateSelectizeInput(
        session,
        "yvalueselect",
        choices = names(pipeline_variables$df_main)
      )
    })

    shiny::observe("set_variables", {
      updateSelectizeInput(
        session,
        "xvalueselect",
        selected = pipeline_variables$var1
      )

      shiny::updateSelectizeInput(
        session,
        "yvalueselect",
        selected = pipeline_variables$var2
      )
    })

    shiny::observeEvent(input$updatevariables, {



        pipeline_variables$newdatacomein(!!rlang::sym(input$xvalueselect),
                                         !!rlang::sym(input$yvalueselect),
                                         !!pipeline_variables$panel_name)

        pipeline_variables$plot_type <- input$plottype

        #print(pipeline_variables$filtereddf)

        session$sendCustomMessage(type = "plottypeflag",
                                  message = input$plottype)

        session$sendCustomMessage(type = "data",
                                  message = list(lines = jsonlite::toJSON(pipeline_variables$newdat),
                                                 centers = jsonlite::toJSON(pipeline_variables$centers)))
        gargoyle::trigger("push_to_tree")

      }, ignoreInit = TRUE)
  })

}

