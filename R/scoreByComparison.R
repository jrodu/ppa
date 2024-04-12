#' UI for score by comparison module
#'
#'
#' @param id id for module
#'
#'
#'
scoreByComparisonUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("compare")))

  # shiny::tagList(
  #   shiny::tags$h4("Select panels by comparison"),
  #   shiny::tags$p("Create a new function by clicking 'create new function'.
  #                 Select function from list, select a panel for comparison,
  #    and click 'select by comparison'.  You
  #                 can change the horizontal line on the ecdf to
  #    increase/decrease panels
  #                 selected.  Click invert to select panels
  #   above/below cutoff.  Click save
  #                 to save functions tried in the session.
  #   Click save and quit to save the
  #                 functions and quit the app."),
  #   newComparisonFunctionUI(ns("fbys")),
  #   shiny::selectizeInput(ns("filterbyselection"), label=NULL,
  #  choices=character(), selected = NULL, multiple = FALSE,
  #                 options = list(placeholder="select comparison function")),
  #   shiny::actionButton(ns("filterSelection"), "select by comparison"),
  #   #### the ecdf plot
  #   plotECDFUI(ns("ecdf")),
  #   shiny::br(),
  #   shiny::actionButton(ns("save"), "Save score functions"),
  #   shiny::actionButton(ns("save_and_quit"), "Save functions and quit")
  # )

}

#' Server for score by comparison module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
scoreByComparisonServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    name <- NULL
    f_compare <- NULL
    fn <- NULL

    ns <- session$ns

    output$compare <- shiny::renderUI(
    shiny::tagList(
      shiny::tags$h4("Select panels by comparison"),
      shiny::p("Create a new function by clicking 'create new function'.
                  To edit an existing function, select the function from
                  your list and click the 'Edit' button.
                  To select panels by the comparison function,
                  select the function from the list,
                  and click 'Select by Comparison'.",
      shiny::strong("In order for the process to run, you must have one,
                  and only one, panel selected prior to
                    pressing 'Select by Comparison'."),
            "After you click this button,
                  an empirical CDF (ecdf) plot of panel comparison scores
                   will appear, as well as a horizontal line indicating a
                   panel selection threshold.
                  Panels are selected when their score falls above/below
                  this threshold.  You can change the threshold level by
                  clicking appropriately
                  on the ecdf plot.
                  You can invert the select (e.g. if panels are selected
                  above or below the threshold) by
                  clicking on Invert Selection below the ecdf plot.
                  Finally, if you hover over a panel,
                  the comparison score will be shown in a tooltip.
                  To label the panels after you are satisfied with the
                  selection, open the 'label' panel"),
      newComparisonFunctionUI(ns("fbys")),
      comparisonbuttonUI(ns("comparisonselectize")),
      # shiny::selectizeInput(ns("filterbyselection"), label=NULL,
      #    choices=character(), selected = NULL, multiple = FALSE,
      #           options = list(placeholder="select comparison function")),
      shiny::actionButton(ns("edit_comparison"), "Edit"),
      shiny::actionButton(ns("filterSelection"), "Select by Comparison"),
      shiny::actionButton(ns("saveScores"), "Save Scores"),
      shiny::actionButton(ns("clearScores"), "Clear Scores"),
      #### the ecdf plot
      #plotECDFUI(ns("ecdf")),
      # shiny::br(),
      # shiny::actionButton(ns("save"), "Save score functions"),
      # shiny::actionButton(ns("save_and_quit"), "Save functions and quit")
      shiny::hr()
    )
    )


    newComparisonFunctionServer("fbys", pipeline_variables)
    compare_fn <- comparisonbuttonServer(
      "comparisonselectize",
      pipeline_variables)

    # shiny::observe({
    #   gargoyle::watch("compare_selectize_update")
    #   shiny::updateSelectizeInput(session,
    #   inputId = "filterbyselection",
    #   choices=pipeline_variables$filter_selection_functions$name,
    #   selected=character())
    # })

    shiny::observeEvent(input$filterSelection, {
      pipeline_variables$reset_filter_criterion()

      session$sendCustomMessage(type = 'clear_invert', message = "clear_invert")

      errorhandle <- NULL

      if(!isTruthy(compare_fn())) {#check if selectize is null
        # fn_txt <- paste('f_compare <- function(panel_data, panel_selected)
        #   {', input$filtercomp, '}', sep='')
        #
        # fn_label <- input$functionName
        # pipeline_variables$filter_selection_functions <-
        #  pipeline_variables$filter_selection_functions %>%
        #  dplyr::add_row(name=fn_label, fn=input$filtercomp)
        errorhandle <- "need to select a function!"
        shinyalert::shinyalert(
          paste0("Oops!",
                 "Code didn't run.  Make sure to select a function first!"),
          type = "error")
      } else{
        fn_txt_tmp <- pipeline_variables$filter_selection_functions %>%
          dplyr::filter(.data$name==compare_fn()) %>% dplyr::select(.data$fn)
        fn_txt <- paste('f_compare <- function(panel_data, panel_selected) {',
                        fn_txt_tmp$fn, '}', sep='')


      tmp_try <- NULL


      tryCatch(eval(parse(text = fn_txt)),
               error = function(e) {
                 errorhandle <<- "there might be an issue with your parsing"})

      if(length(pipeline_variables$cur_selection)!=1){
        errorhandle <- "need to select a panel!"
        shinyalert::shinyalert(
          paste0(
            "Oops!",
            "Code didn't run.  Make sure to select one--and only one-- panel!"),
          type = "error")
      }else{

      if(is.null(errorhandle)){
        base_panel <- pipeline_variables$cur_selection
        tryCatch(
            tmp_try <- get_comparison_scores(
          pipeline_variables$df_main,
          pipeline_variables$cur_selection, f_compare) %>%
            dplyr::mutate(use_score=1, use_pos=0),
                 error = function(e) {
                   errorhandle <<- "your code is throwing an error"},
                 warning=function(w) {
                   errorhandle <<- "throwing some warnings here..."})

      if(is.null(errorhandle)){
      pipeline_variables$filtereddf <- tmp_try %>% dplyr::mutate(name=paste0(compare_fn(), '_', base_panel))

      tryCatch(scores <-  pipeline_variables$filtereddf %>%
                 dplyr::filter(
                   .data$panel_string !=  pipeline_variables$cur_selection),
               error = function(e) {
                 errorhandle <<- "your code is throwing an error"},
               warning=function(w) {
                 errorhandle <<- "throwing some warnings here..."})
      if(is.null(errorhandle)){
      cutoff <- stats::quantile(scores$score, probs=.05)
      filter_cutoff <-  pipeline_variables$filtereddf %>%
        dplyr::filter(.data$score<cutoff)
      new_centers <- pipeline_variables$centers
      new_centers <- new_centers %>% dplyr::select(-.data$score) %>%
        dplyr::left_join(pipeline_variables$filtereddf, by="panel_string") %>%
        dplyr::mutate(use_score=1) %>%
        dplyr::mutate(stroke_width = ifelse(panel_string==base_panel, 5, 1))

      shiny::updateTextInput(session, "functionName", value=character())
      shiny::updateTextAreaInput(session, "filtercomp", value=character())
      shiny::updateSelectizeInput(
        session, inputId = "filterbyselection",
        choices=pipeline_variables$filter_selection_functions$name,
        selected=character())
      shiny::updateCheckboxInput(session, "newfunction", value=FALSE)


      #compute ecdf needs:
      buffer <- ggplot2::resolution(scores$score, zero=FALSE)*3
      x <- sort(unique(scores$score))
      x <- c(min(x)-buffer, x, max(x)+buffer)
      ecdf_data <- stats::ecdf(scores$score)(x)


      #send the selection to the main panel
      session$sendCustomMessage(
        type = 'panelPlot_set',
        message = list(centers=jsonlite::toJSON(new_centers),
        sel = jsonlite::toJSON(filter_cutoff$panel_string)))

      #print(new_centers$use_score)
      #send data for ecdf.js to render
      session$sendCustomMessage(
        type = "data_ecdf",
        message = jsonlite::toJSON(tibble::tibble(x=x, y=ecdf_data)))
    }else{
      shinyalert::shinyalert(
        paste0("Oops!",
               "Code didn't run.  Perhaps there is a typo in your function?
               Maybe this is a hint: ",
               errorhandle,
               ". Select the function and click 'edit' to edit the function."),
        type = "error")
    }
      }else{
        shinyalert::shinyalert(
          paste0("Oops!",
                 "Code didn't run.  Perhaps there is a typo in your function?
                 Maybe this is a hint: ",
                 errorhandle,
                 ". Select the function and click 'edit'
                 to edit the function."),
          type = "error")
      }
      }else{
        shinyalert::shinyalert(
          paste0("Oops!",
                 "Code didn't run.  Perhaps there is a typo in your function?
                 Maybe this is a hint: ",
                 errorhandle,
                 ". Select the function and click 'edit'
                 to edit the function."),
          type = "error")
      }
      }
      }

    })

    ####### save scores

    shiny::observeEvent(input$saveScores, {

      #if(isTruthy(input$filterselectize)){
      pipeline_variables$update_scores(pipeline_variables$filtereddf$name %>% base::unique())
      #}

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$edit_comparison, {

      if(!isTruthy(compare_fn())) {#check if selectize is null
        # fn_txt <- paste('f_compare <- function(panel_data, panel_selected)
        #   {', input$filtercomp, '}', sep='')
        #
        # fn_label <- input$functionName
        # pipeline_variables$filter_selection_functions <-
        #  pipeline_variables$filter_selection_functions %>%
        #  dplyr::add_row(name=fn_label, fn=input$filtercomp)
        errorhandle <- "need to select a function!"
        shinyalert::shinyalert(
          paste0("Oops!",
                 "Make sure to select a function to edit first!"),
          type = "error")
      } else{

      shinyalert::shinyalert(
        html = TRUE,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showConfirmButton = FALSE,
        text = tagList(
          shinyjs::disabled(
            textInput(ns("editComparisonName"),
                      label=NULL, value=compare_fn())),
          textAreaInput(ns("editcomparisonval"),
                        label="function(panel_data, panel_selected) {",
                        value=pipeline_variables$filter_selection_functions %>%
                          dplyr::filter(.data$name==compare_fn()) %>%
                          dplyr::select(.data$fn),
                        width="100%", height="300px"), strong("}"),
                               shiny::br(),
                               shiny::actionButton(
                                 ns("submitedcomparisonfunction"),
                                 "confirm edit")
                             ))
      }

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$submitedcomparisonfunction, {


      fn_label <- input$editComparisonName

      pipeline_variables$filter_selection_functions <-
        pipeline_variables$filter_selection_functions %>%
        dplyr::mutate(
          fn=base::replace(.data$fn,
                           .data$name==fn_label,
                           input$editcomparisonval))


      shiny::updateTextInput(session, "editComparisonName", value=character())
      shiny::updateTextAreaInput(
        session,
        "editcomparisonval",
        value=character())


      gargoyle::trigger("compare_selectize_update")

    })

    shiny::observeEvent(input$clearScores, {

      pipeline_variables$define_filtered()
      session$sendCustomMessage(
        type = 'clear_ecdf',
        message = 'clear')
      session$sendCustomMessage(
        type = 'panelPlot_set',
        message = list(centers=jsonlite::toJSON(pipeline_variables$centers),
                       sel=character(0)))


    }, ignoreInit = TRUE)

    # new shiny clear comparison button
    # clear_ecdf through javascript message handler
    #define_filtered()
    #send new selections (none) to canvas (maybe try without this?  Should fail.)


    # shiny::observeEvent(input$save | input$save_and_quit, {
    #   compare_functions <<- pipeline_variables$filter_selection_functions
    # }, ignoreInit = TRUE)
    # shiny::observeEvent(input$save_and_quit, {
    #   shiny::stopApp()
    # }, ignoreInit = TRUE)
  })

}

