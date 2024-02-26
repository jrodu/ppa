#' Call the ppa app
#'
#' @param mydata (required) The dataset containing the data being plotted
#'
#' @param x (required) variable name to be mapped to the x axis
#' @param y (required) variable name to be mapped to the y axis
#' @param panel (required) variable that indicates the specific panel to which
#'              data belong
#' @param plottype currently either 'line' (default) or 'scatter' which defines
#'              the plot type
#' @param rowgroup variable that groups panels by row if applicable
#' @param colgroup variable that groups panels by column if applicable
#' @param randomize currently unused
#' @param grid tibble with columns ROW, COL, and the 'panel' variable which
#'              allows for a customized grid
#' @param session_name name for the session
#' @param ... additional variables to be passed on
#'
#' @export
#'
ppa <- function(mydata, x, y, panel, plottype, rowgroup, colgroup,
                randomize = NA, grid = NULL,session_name="ppa",...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  panel <- rlang::enquo(panel)

  panel.type <- class(mydata %>% dplyr::pull(!!panel))

  if(missing(plottype)){
    plottype <- 'line'
  }

  if(missing(rowgroup)){
    use_row <- FALSE
    if(missing(colgroup)){
      use_col <- FALSE
    } else {
      use_col <- TRUE
      colgroup <- rlang::enquo(colgroup)
      grid <- mydata %>% get_rowcol(colgroup=!!colgroup, panelgroup=!!panel)
    }
  } else {
    use_row <- TRUE
    rowgroup <- rlang::enquo(rowgroup)
    if(missing(colgroup)){
      use_col <- FALSE
      grid <- mydata %>% get_rowcol(rowgroup=!!rowgroup, panelgroup=!!panel)
    } else{
      use_col <- TRUE
      colgroup <- rlang::enquo(colgroup)
      grid <- mydata %>% get_rowcol(rowgroup=!!rowgroup, colgroup=!!colgroup,
                                    panelgroup=!!panel)
    }
  }


  ui <- shiny::fluidPage(
    tags$head(
      tags$style(
        HTML(
          "
          .form-group {
            margin-bottom: 0 !important;
          }

          .checkbox {
            margin-bottom: 0 !important;
            margin-top: 0 !important;
          }
        "
        )
      )
    ),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("hr {border-top: 1px solid #000000;}"))
    ),
    shiny::fluidRow(shiny::column(9,
                      style='border-right:1px solid grey;padding-bottom:50px',
                      shiny::tags$script(src='https://d3js.org/d3.v6.min.js'),
   panelsUI("mainpanel")),
  shiny::column(3, style='padding-top:30px;',


                #shiny::tags$h4("Panel Settings"),
  ###### axes
  shiny::tags$p("Select up to 3 at a time. 'Score' and 'Comparison' cannot
                be selected simultaneously."),
  shiny::checkboxInput("viewVariables", "Individual Panel Settings", FALSE),
  #tooltip("viewVariables", "tooltip tester"),
  shiny::checkboxInput("viewShuffle", "Panel Shufflers", FALSE),
  shiny::checkboxInput("viewSelectors", "Panel Selection Actions", FALSE),
  shiny::checkboxInput("excludePanels", "Exclude/Unexclude Panels", FALSE),
  #shiny::checkboxInput("examinePanels", "Examine Panels", FALSE),
  shiny::checkboxInput("labelPanels",
                       "Label Panels and Select by Label", FALSE),
  shiny::checkboxInput("scorePanels",
                       "Select Panels by a Score Function", FALSE),
  shiny::checkboxInput("comparePanels", "Select Panels by Comparison", FALSE),
  shiny::checkboxInput("placePanels", "Arrange Panels in Scatter Plot", FALSE),
  shiny::checkboxInput("transformPanels", "Transform Panels", FALSE),
  shiny::hr(),
  shiny::tags$img(id = "fake1", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake2", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake3", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake4", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake5", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake6", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake7", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake8", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake9", src =NA, width = "0px", height = "0px"),
  shiny::tags$img(id = "fake10", src =NA, width = "0px", height = "0px")

  )
    )
  )



  server <- function(input, output, session) {
    gargoyle::init("print_debug", "send_to_panel_plot",
                   "filter_one", "push_to_tree", "filter_chain",
                   "save_session", "update_variables", "set_variables",
                   "compare_selectize_update",
                   "filter_selectize_update", "transform_selectize_update")

    shiny::onStop(function() {

      # base::save(mvar, file="tmp_save_variables.rda")
      # load("tmp_save_variables.rda", envir = .GlobalEnv)
      # file.remove("tmp_save_variables.rda")
      keepvars <- save_vars(pipeline_variables, session_name)
      base::list2env(keepvars, .GlobalEnv)
      shiny::stopApp()
      })

    output$modules <- reactive({
      1
    })
    output$value1 <- renderText({ input$switch1 })

    mylist <- list()

    pipeline_variables <- PanelObject$new()
    pipeline_variables$load_data <- mydata
    pipeline_variables$randomize <- randomize
    pipeline_variables$panel_data_type <- panel.type




    if(length(grid)==3){
      pipeline_variables$setup_grid <- grid
    }
    pipeline_variables$newdatacomein(!!x, !!y, !!panel)

    pipeline_variables$plot_type <- plottype



    panelsServer("mainpanel", pipeline_variables)

    session$sendCustomMessage(type = "plottypeflag",
                              message = plottype)
    ######################### axes ###################################

    shiny::insertUI(
      selector = "#fake1",
      where = "afterEnd",
      ui = axesUI("axes")
    )


    maxchecks <- function(){
      totchecked <- input$viewVariables + input$viewShuffle +
        input$viewSelectors +
        input$excludePanels + input$labelPanels + input$scorePanels +
        input$comparePanels + input$placePanels + input$transformPanels
      if(totchecked==3){
        if(!input$viewVariables) shinyjs::disable("viewVariables")
        if(!input$viewShuffle) shinyjs::disable("viewShuffle")
        if(!input$viewSelectors) shinyjs::disable("viewSelectors")
        if(!input$excludePanels) shinyjs::disable("excludePanels")
        if(!input$labelPanels) shinyjs::disable("labelPanels")
        if(!input$scorePanels) shinyjs::disable("scorePanels")
        if(!input$comparePanels) shinyjs::disable("comparePanels")
        if(!input$placePanels) shinyjs::disable("placePanels")
        if(!input$transformPanels) shinyjs::disable("transformPanels")
      }
      if(totchecked<3){
        shinyjs::enable("viewVariables")
        shinyjs::enable("viewShuffle")
        shinyjs::enable("viewSelectors")
        shinyjs::enable("excludePanels")
        shinyjs::enable("labelPanels")
        if(input$comparePanels){
            shinyjs::disable("scorePanels")
          }else{
            shinyjs::enable("scorePanels")}
        if(input$scorePanels){
          shinyjs::disable("comparePanels")
          }else{
            shinyjs::enable("comparePanels")
          }
        shinyjs::enable("placePanels")
        shinyjs::enable("transformPanels")
      }
    }

    observeEvent(input$viewVariables, {
      if(input$viewVariables){
        shinyjs::show(id="axes-axes")
        }else{
          shinyjs::hide(id="axes-axes")
      }
      maxchecks()



    })

    axesServer("axes", pipeline_variables)

    ######################### shuffle ###################################

    shiny::insertUI(
      selector = "#fake2",
      where = "afterEnd",
      ui = shufflePanelUI("shuffle")
    )

    observeEvent(input$viewShuffle, {
      if(input$viewShuffle){
        shinyjs::show(id="shuffle-shuffle")
      }else{
        shinyjs::hide(id="shuffle-shuffle")
      }
      maxchecks()



    })

    shufflePanelServer("shuffle", pipeline_variables)

    ######################### selectors ###################################

    shiny::insertUI(
      selector = "#fake3",
      where = "afterEnd",
      ui = unselectAllUI("unselectpanels")
    )

    observeEvent(input$viewSelectors, {
      if(input$viewSelectors){
        shinyjs::show(id="unselectpanels-unselect")
      }else{
        shinyjs::hide(id="unselectpanels-unselect")
      }
      maxchecks()

    })

    unselectAllServer("unselectpanels", pipeline_variables)


    ######################### exclude ###################################

    shiny::insertUI(
      selector = "#fake4",
      where = "afterEnd",
      ui = excludeUI("exclude")
    )

    observeEvent(input$excludePanels, {
      if(input$excludePanels){
        shinyjs::show(id="exclude-exclude")
      }else{
        shinyjs::hide(id="exclude-exclude")
      }
      maxchecks()

      })

    excludeServer("exclude", pipeline_variables)


    ######################### ecdf ###################################

    shiny::insertUI(
      selector = "#fake8",
      where = "afterEnd",
      ui = plotECDFUI("ecdf")
    )

    observeEvent(input$scorePanels | input$comparePanels, {
      if(input$scorePanels | input$comparePanels){
        shinyjs::show(id="ecdf-ecdf")
      }else{
        shinyjs::hide(id="ecdf-ecdf")
      }
      maxchecks()

    })

    plotECDFServer("ecdf", pipeline_variables)

    ######################### score ###################################

    shiny::insertUI(
      selector = "#fake6",
      where = "afterEnd",
      ui = scoreByFunctionUI("score")
    )

    observeEvent(input$scorePanels, {
      if(input$scorePanels){
        shinyjs::show(id = "score-scorebyfunction")
      }else{
        shinyjs::hide(id = "score-scorebyfunction")
      }
      maxchecks()
    })

    scoreByFunctionServer("score", pipeline_variables)


    ######################### compare ###################################

    shiny::insertUI(
      selector = "#fake7",
      where = "afterEnd",
      ui = scoreByComparisonUI("compare")
    )

    observeEvent(input$comparePanels, {
      if(input$comparePanels){
        shinyjs::show(id="compare-compare")
      }else{
        shinyjs::hide(id="compare-compare")
      }
      maxchecks()
    })

    scoreByComparisonServer("compare", pipeline_variables)



    ######################### label ###################################

    shiny::insertUI(
      selector = "#fake5",
      where = "afterEnd",
      ui = labelUI("label")
    )

    observeEvent(input$labelPanels, {
      if(input$labelPanels){
        shinyjs::show(id="label-label")
      }else{
        shinyjs::hide(id="label-label")
      }

      maxchecks()
    })

    labelServer("label", pipeline_variables)



    ######################### place ###################################

    shiny::insertUI(
      selector = "#fake9",
      where = "afterEnd",
      ui = placePanelsUI("place")
    )

    observeEvent(input$placePanels, {
      if(input$placePanels){
        shinyjs::show(id="place-place")
      }else{
        shinyjs::hide(id="place-place")
      }

      maxchecks()
    })

    placePanelsServer("place", pipeline_variables)


    ######################### Transform ###################################

    shiny::insertUI(
      selector = "#fake10",
      where = "afterEnd",
      ui = transformUI("transform")
    )


    observeEvent(input$transformPanels, {
      if(input$transformPanels){
        shinyjs::show(id="transform-transform")
      }else{
        shinyjs::hide(id="transform-transform")
      }
      maxchecks()

    })

    transformServer("transform", pipeline_variables)

    ######################### END ###################################

  }
  shiny::shinyApp(ui, server, ...)
}
