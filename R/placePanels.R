#' UI for panel placement module
#'
#'
#' @param id id for module
#'
#'
#'
placePanelsUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("place")))

  # shiny::tagList(
  #   newFilterFunctionUI(ns("fbyv")),
  #   shiny::selectizeInput(ns('fn1'), label=NULL, choices=character(),
  #                  options = list(create = TRUE,
  # placeholder="Select placement in x1 direction"),
  #                  multiple=FALSE), shiny::br(),
  #   shiny::selectizeInput(ns('fn2'), label=NULL, choices=character(),
  #                  options = list(create = TRUE,
  # placeholder="Select placement in x2 direction"),
  #                  multiple=FALSE),
  #   shiny::actionButton(ns("reposition"), "place panels"),
  #   shiny::actionButton(ns("returnToGrid"), "return panels to grid")
  # )

}

#' Server for panel placement module
#'
#' @param id id for examine module
#' @param pipeline_variables R6 global variable object
#'
#'
placePanelsServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    fn <- NULL

    ns <- session$ns

    output$place <- shiny::renderUI(
      shiny::tagList(
        shiny::tags$h4("Arrange Panels in Scatter Plot"),
        shiny::tags$p(
        "This functionality uses score functions
        (i.e. selection by score function) to arrange
        the panels in a scatterplot formation.
        Functions you create here will be available in the
        select by score function panel, and functions created
        in that panel will be available here.
        To edit a function, select it in the
        'select x1 function' menu, and click 'edit'.
                      "),
        newFilterFunctionUI(ns("fbyv")), shiny::actionButton(ns("edit_placer"),
                                                             "Edit"),
        placerbuttonUI(ns("fn1"), "x1"),
        placerbuttonUI(ns("fn2"), "x2"),
        # shiny::selectizeInput(ns('fn1'), label=NULL, choices=character(),
        #   options = list(create = TRUE,
        #  placeholder="Select placement in x1 direction"),
        #                       multiple=FALSE), shiny::br(),
        # shiny::selectizeInput(ns('fn2'), label=NULL, choices=character(),
        #                       options = list(create = TRUE,
        #   placeholder="Select placement in x2 direction"),
        #                       multiple=FALSE),
        shiny::actionButton(ns("reposition"), "place panels in scatter plot"),
        shiny::actionButton(ns("returnToGrid"), "return panels to grid"),
        shiny::hr()
        )
    )

    outputOptions(output, "place", suspendWhenHidden = FALSE)

    newFilterFunctionServer("fbyv", pipeline_variables)
    filter_fn1 <- placerbuttonServer("fn1", pipeline_variables)
    filter_fn2 <- placerbuttonServer("fn2", pipeline_variables)

    # shiny::observe({
    #   gargoyle::watch("filter_selectize_update")
    #   shiny::updateSelectizeInput(session, inputId = ns("fn1"),
    #   choices=pipeline_variables$filter_value_functions$name,
    #      selected=character())
    #   shiny::updateSelectizeInput(session, inputId = ns("fn2"),
    #    choices=pipeline_variables$filter_value_functions$name,
    #    selected=character())
    #
    # })

    shiny::observeEvent(input$reposition, {



      fn1_txt_tmp <- pipeline_variables$filter_value_functions %>%
        dplyr::filter(.data$name==filter_fn1()) %>% dplyr::select(.data$fn)
      fn1_txt <- paste('f_compare <- function(panel_data) {',
                       fn1_txt_tmp$fn, '}', sep='')

      fn2_txt_tmp <- pipeline_variables$filter_value_functions %>%
        dplyr::filter(.data$name==filter_fn2()) %>% dplyr::select(fn)
      fn2_txt <- paste('f_compare <- function(panel_data) {',
                       fn2_txt_tmp$fn, '}', sep='')

      #fn1_txt <- "f_compare <- function(panel_data) {max(panel_data$cadaba)}"
      #fn2_txt <- "f_compare <- function(panel_data) {min(panel_data$cadaba)}"


      tmp_try <- NULL

      try(tmp_try <- get_value_scores(
        pipeline_variables$df_main,
        eval(parse(text = fn1_txt)),
        eval(parse(text = fn2_txt))) %>%
          dplyr::mutate(use_score=0, use_pos=1))

      if(!is.null(tmp_try)){

      pipeline_variables$filtereddf <- tmp_try

      width_repositioned <- pipeline_variables$centers[1,]$width
      height_repositioned <- pipeline_variables$centers[1,]$height
      new_centers <- get_new_plot_vals(pipeline_variables$centers,
                                       pipeline_variables$filtereddf)

      new_centers <- new_centers %>% dplyr::mutate(
        width=width_repositioned,
        height = height_repositioned)
      center_dist <- get_centers_dist(
        new_centers %>%
          dplyr::select(.data$dotcx, .data$dotcy, .data$panel_string),
        width_repositioned, height_repositioned)

      #print(center_dist %>% filter(num_overlap<10))

      filter_this <- center_dist %>% dplyr::filter(.data$num_overlap<4) %>%
        dplyr::select(.data$panel_string)

      new_centers2 <- new_centers %>%
        dplyr::filter(
          .data$panel_string %in% as.vector(filter_this$panel_string)
          )


      new_points <- new_centers %>%
        dplyr::filter(
          !.data$panel_string %in% as.vector(filter_this$panel_string)
          )

      #print(new_points)

      session$sendCustomMessage(
        type = "data_new_centers",
        message = list(centers = jsonlite::toJSON(new_centers),
        points = jsonlite::toJSON(new_points),
        centers_use = jsonlite::toJSON(new_centers2)))

      }else{
        shinyalert::shinyalert("Oops!", "Code didn't run.  Perhaps there is a
              typo in on of your functions?  Using input function 1,
              select a function and click 'edit' to edit the function.",
                               type = "error")
      }


    })

    shiny::observeEvent(input$edit_placer, {

      shinyalert::shinyalert(
        html = TRUE,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showConfirmButton = FALSE,text = tagList(
        shinyjs::disabled(
          textInput(ns("editPlacerName"), label=NULL, value=filter_fn1())
          ),
          textAreaInput(ns("editplacerval"),
          label="function(panel_data) {",
          value=pipeline_variables$filter_value_functions %>%
          dplyr::filter(.data$name==filter_fn1()) %>% dplyr::select(.data$fn),
          width="100%", height="300px"), strong("}"),
          shiny::br(),
          shiny::actionButton(ns("submitedplacerfunction"), "confirm edit")
                             ))

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$submitedplacerfunction, {


      fn_label <- input$editPlacerName
      pipeline_variables$filter_value_functions <-
        pipeline_variables$filter_value_functions %>%
        dplyr::mutate(fn=base::replace(
          .data$fn,
          .data$name==fn_label,
          input$editplacerval))


      shiny::updateTextInput(session, "editPlacerName", value=character())
      shiny::updateTextAreaInput(session, "editplacerval", value=character())


      gargoyle::trigger("filter_selectize_update")

    })

    observeEvent(input$returnToGrid, {
      pipeline_variables$filtereddf$use_pos <- 0
      gargoyle::trigger("send_to_panel_plot")

      updateSelectizeInput(
        session,
        inputId = "fn1",
        choices=pipeline_variables$filter_value_functions$name,
        selected=character())
      updateSelectizeInput(
        session,
        inputId = "fn2",
        choices=pipeline_variables$filter_value_functions$name,
        selected=character())
    })

  })

}

