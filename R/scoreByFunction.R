#' UI for score by function module
#'
#'
#' @param id id for module
#'
#'
#'
scoreByFunctionUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("scorebyfunction")))

  # shiny::tagList(
  #   shiny::tags$h4("Select panels by score"),
  #   shiny::tags$p("Create a new function by clicking 'create new function'.
  #                 Select function from list, and click 'select by value'.  You
  #                 can change the horizontal line on the ecdf to
  #   increase/decrease panels
  #                 selected.  Click invert to select panels
  #   above/below cutoff.  Click save
  #                 to save functions tried in the session.
  #  Click save and quit to save the
  #                 functions and quit the app."),
  #   newFilterFunctionUI(ns("fbyv")),
  #   filterbuttonUI(ns("filterselectize")),
  #   shiny::actionButton(ns("filterValue"), "select by value"),
  #   #### the ecdf plot
  #     plotECDFUI(ns("ecdf")),
  #   shiny::br(),
  #   shiny::actionButton(ns("save"), "Save score functions"),
  #   shiny::actionButton(ns("save_and_quit"), "Save functions and quit")
  # )

}

#' Server for score by function module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
scoreByFunctionServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    name <- NULL
    f_compare <- NULL
    fn <- NULL

    ns <- session$ns

    output$scorebyfunction <- shiny::renderUI(
      #selector = paste0("#", ns("putaxes")),
      #where = "beforeBegin",
      shiny::tagList(
        shiny::tags$h4("Select panels by a score function"),
        shiny::tags$p("Create a new function by clicking 'create new function'.
                  To edit an existing function, select the function from
                  your list and click the 'Edit' button.
                  To select panels by the score function,
                  select the function from the list,
                  and click 'Select by Score'.  After you click this button,
                  an empirical CDF (ecdf) plot of panel score
                  values will appear, as well as a horizontal line indicating
                  a panel selection threshold.
                  Panels are selected when their score falls above/below
                  this threshold.  You can change the threshold level by
                  clicking appropriately
                  on the ecdf plot.
                  You can invert the select (e.g. if panels are selected above
                  or below the threshold) by
                  clicking on Invert Selection below the ecdf plot.
                  Finally, if you hover over a panel,
                  the comparison score will be shown in a tooltip.
                  To label the panels after you are satisfied with the
                  selection, open the 'label' panel"),
        newFilterFunctionUI(ns("fbyv")),
        filterbuttonUI(ns("filterselectize")),
        shiny::actionButton(ns("edit_filter"), "Edit"),
        shiny::actionButton(ns("filterValue"), "Select by Score"),
        shiny::actionButton(ns("saveScores"), "Save Scores"),
        shiny::actionButton(ns("clearScores"), "Clear Scores"),
        # shiny::br(),
        # shiny::actionButton(ns("save"), "Save score functions"),
        # shiny::actionButton(ns("save_and_quit"), "Save functions and quit")
        shiny::hr()
      )
    )

    outputOptions(output, "scorebyfunction", suspendWhenHidden = FALSE)

    newFilterFunctionServer("fbyv", pipeline_variables)
    filter_fn <- filterbuttonServer("filterselectize", pipeline_variables)


    shiny::observeEvent(input$filterValue, {
      pipeline_variables$reset_filter_criterion()

      errorhandle <- NULL

      session$sendCustomMessage(type = 'clear_invert', message = "clear_invert")

      if(!isTruthy(filter_fn())) {#check if selectize is null
        # fn_txt <- paste('f_compare <-
        #  function(panel_data) {', input$filterval, '}', sep='')
        #
        # fn_label <- input$functionValueName
        # pipeline_variables$filter_value_functions <-
        #  pipeline_variables$filter_value_functions %>%
        #  dplyr::add_row(name=fn_label, fn=input$filterval)
        errorhandle <- "need to select a function!"
        shinyalert::shinyalert(
          paste0("Oops!",
                 "Code didn't run.  Make sure to select a function first!"),
          type = "error")
      } else{
        fn_txt_tmp <- pipeline_variables$filter_value_functions %>%
          dplyr::filter(.data$name==filter_fn()) %>% dplyr::select(.data$fn)
        fn_txt <- paste('f_compare <- function(panel_data) {',
                        fn_txt_tmp$fn, '}',
                        sep='')

      tmp_try <- NULL


      tryCatch(
        eval(
          parse(text = fn_txt)
          ),
        error = function(e) {
          errorhandle <<- "there might be an issue with your parsing"})

      if(is.null(errorhandle)){
      tryCatch(
        tmp_try <- get_value_scores(pipeline_variables$df_main, f_compare) %>%
          dplyr::mutate(use_score=1, use_pos=0),
               error = function(e) {
                 errorhandle <<- "your code is throwing an error"},
               warning=function(w) {
                 errorhandle <<- "throwing some warnings here..."})

      if(is.null(errorhandle)){
      pipeline_variables$filtereddf <-tmp_try %>% dplyr::mutate(name=filter_fn())
      cutoff <- stats::quantile(pipeline_variables$filtereddf$score, probs=.05)

      filter_cutoff <- pipeline_variables$filtereddf %>%
        dplyr::filter(.data$score<cutoff)
      new_centers <- pipeline_variables$centers
      new_centers <- new_centers %>% dplyr::select(-.data$score) %>%
        dplyr::left_join(pipeline_variables$filtereddf, by="panel_string") %>%
        dplyr::mutate(use_score=1)

      gargoyle::trigger("filter_selectize_update")


      #compute ecdf needs:
      buffer <- ggplot2::resolution(
        pipeline_variables$filtereddf$score, zero=FALSE)*3
      x <- sort(unique(pipeline_variables$filtereddf$score))
      x <- c(min(x)-buffer, x, max(x)+buffer)
      ecdf_data <- stats::ecdf(pipeline_variables$filtereddf$score)(x)


      #send the selection to the main panel
      session$sendCustomMessage(
        type = 'panelPlot_set',
        message = list(
          centers=jsonlite::toJSON(new_centers),
          sel = jsonlite::toJSON(filter_cutoff$panel_string)))

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
                 ". Select the function and click 'edit' to
                 edit the function."),
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

    })

    ####### save scores

    shiny::observeEvent(input$saveScores, {

      #if(isTruthy(input$filterselectize)){
        pipeline_variables$update_scores(pipeline_variables$filtereddf$name %>% base::unique())
      #}

    }, ignoreInit = TRUE)


    shiny::observeEvent(input$edit_filter, {
      if(!isTruthy(filter_fn())) {#check if selectize is null
        # fn_txt <- paste('f_compare <-
        #  function(panel_data) {', input$filterval, '}', sep='')
        #
        # fn_label <- input$functionValueName
        # pipeline_variables$filter_value_functions <-
        #  pipeline_variables$filter_value_functions %>%
        #  dplyr::add_row(name=fn_label, fn=input$filterval)
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
            textInput(
              ns("editFilterName"),
              label=NULL,
              value=filter_fn())),
          textAreaInput(
            ns("editfilterval"),
            label="function(panel_data) {",
            value=pipeline_variables$filter_value_functions %>%
              dplyr::filter(.data$name==filter_fn()) %>%
              dplyr::select(.data$fn),
            width="100%", height="300px"), strong("}"),
          shiny::actionButton(ns("submitedfilterfunction"), "confirm edit")
                             ))
      }

    }, ignoreInit = TRUE)



    shiny::observeEvent(input$submitedfilterfunction, {


      fn_label <- input$editFilterName

      pipeline_variables$filter_value_functions <-
        pipeline_variables$filter_value_functions %>%
        dplyr::mutate(
          fn=base::replace(.data$fn, .data$name==fn_label, input$editfilterval))


      shiny::updateTextInput(session, "editFilterName", value=character())
      shiny::updateTextAreaInput(session, "editfilterval", value=character())


      gargoyle::trigger("filter_selectize_update")

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

        # shiny::observeEvent(input$save | input$save_and_quit, {
        #   score_functions <<- pipeline_variables$filter_value_functions
        # }, ignoreInit = TRUE)
        # shiny::observeEvent(input$save_and_quit, {
        #   shiny::stopApp()
        # }, ignoreInit = TRUE)
  })

}

