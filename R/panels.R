#' UI for panels module
#'
#'
#' @param id id for module
#'
#'
#'
panelsUI <- function(id) {

  ns <- shiny::NS(id)
#
   shiny::fluidRow(shiny::column(12,

  shiny::tagList(
    shiny::tags$head(
    shiny::tags$style(shiny::HTML(paste0("#", ns("tooltip"), " {
            position: absolute;
            width: auto;
            height: auto;
            padding: 10px;
    background-color: cyan;
    -webkit-border-radius: 10px;
    -moz-border-radius: 10px;
    border-radius: 10px;
    -webkit-box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
    -moz-box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
    box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
    pointer-events: none;
}

#", ns("tooltip"), ".hidden {
    display: none;
}

#", ns("tooltip"), " p {
    margin: 0;
    font-family: sans-serif;
    font-size: 16px;
    line-height: 20px;
}"))))),
  shiny::uiOutput(ns("pans_controls")),
  shiny::uiOutput(ns("pans")),


  shiny::br()
))
}

#' Not used?
#'
#' @param expr expression to execute
#' @param session session
#'
execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
  observeEvent(once = TRUE, reactiveValuesToList(session$input), {
    force(expr)
  }, ignoreInit = TRUE)
}


#' Server for panels module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
panelsServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){

    ns <- session$ns

    height_ratio <- reactiveVal(1)


     output$pans <- shiny::renderUI(
        #pansUI(ns("sticka"))
       shiny::tagList(
         shiny::tags$div(
           id = ns("d3_output"),
           class="svg-container",
           style =
             paste0("width: 100%;height:",
                    height_ratio()*1000,"px;padding-top: 50px")
         ),
         shiny::tags$div(
           id = ns("tooltip"),
           class =
             "hidden",
           list(shiny::tags$p(shiny::tags$span(id = ns(
             "value"
           ),
           100)))
         ),
         shiny::tags$script(shiny::HTML(
           paste0("var
       stickpltdiv=d3.select('#", ns("d3_output"), "');")
         )),
         shiny::tags$script(shiny::HTML(
           paste0("var plotDiv = stickpltdiv.node();")
         )),
         shiny::tags$script(shiny::HTML(
           paste0("var svg = stickpltdiv
         .append('svg')")
         )),

         shiny::tags$script(shiny::HTML(
           paste0("var toolt=d3.select('#",
                  ns("tooltip"), "');")
         )),
         shiny::tags$script(shiny::HTML(paste0(
           "var
       pclick='", ns("panel_clicked"), "';"
         ))),
         shiny::includeScript(path = system.file("js", "panels.js", package =
                                                   "ppa")),
         shiny::tags$script(shiny::HTML(
           paste0("plotflag = '", pipeline_variables$plot_type, "';"))),
         shiny::tags$script(shiny::HTML(
           paste0("redraw();")
         ))

       ))



     output$pans_controls <- shiny::renderUI(

       shiny::actionButton(ns("panelproperties"), "panel plot properties")
    )

     observeEvent(input$panelproperties, {
       shinyalert::shinyalert(
         html = TRUE,
         closeOnEsc = TRUE,
         closeOnClickOutside = TRUE,
         showConfirmButton = FALSE,
         text = tagList(
           shiny::numericInput(ns("numcol"), "number of columns",
           value = max(pipeline_variables$setup_grid$COL), min = 0, max = 100),
           shiny::numericInput(
             ns("multiplier"), "change vertical size",
             value = isolate(height_ratio()), min = 0, max = 100),
             shiny::actionButton(ns("updatepanels"), "update panels")
                                ))

     }, ignoreInit = TRUE)

    # #
    session$onFlushed(function() {
     session$sendCustomMessage(
       type = "data",
       message = list(lines = jsonlite::toJSON(pipeline_variables$newdat),
       centers = jsonlite::toJSON(pipeline_variables$centers)))

   })

    shiny::observeEvent(input$updatepanels, {
      old_height_ratio <- isolate(height_ratio())
      if((input$multiplier - old_height_ratio)!=0){
        height_ratio(input$multiplier)
      }
      old_colnums <- max(pipeline_variables$setup_grid$COL)
      if((input$numcol-old_colnums)!=0){
      pipeline_variables$maxcol <- input$numcol
      pipeline_variables$force_grid <- TRUE
      pipeline_variables$setup_grid <- NULL
      pipeline_variables$trigger_newdat()
      }
      gargoyle::trigger("send_to_panel_plot")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$panel_clicked, {
      pipeline_variables$selected_state <- input$panel_clicked
    }, ignoreInit = TRUE)

    shiny::observe(priority=-5, {
      gargoyle::watch("send_to_panel_plot")

      session$sendCustomMessage(
        type = "data",
        message = list(lines = jsonlite::toJSON(pipeline_variables$newdat),
        centers = jsonlite::toJSON(pipeline_variables$centers)))


    })

  })
}
