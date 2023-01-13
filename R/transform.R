#' UI for transform module
#'
#'
#' @param id id for module
#'
#' @importFrom magrittr %>%
#'
#'
transformUI <- function(id) {

  ns <- shiny::NS(id)

  shinyjs::hidden(uiOutput(ns("transform")))

  # shiny::tagList(
  #   newTransformFunctionUI(ns("tform")),
  #   shiny::selectizeInput(ns('trnsfm'), label=NULL, choices=character(),
  #                  options = list(create = TRUE, placeholder="Select a previously defined transform function"),
  #                  multiple=FALSE),
  #   shiny::br(),
  #
  #   shiny::actionButton(ns("transform"), "transform data"),
  #
  #   transformTrackerUI(ns("tformtrack"))
  # )

}

#' Server for transform module
#'
#' @param id id for module
#' @param pipeline_variables R6 global variable object
#'
#'
transformServer <- function(id, pipeline_variables) {
  shiny::moduleServer(id, function(input, output, session){
    name <- NULL
    fn <- NULL
    f_transform <- NULL
    parentID <- NULL
    type <- NULL
    ns <- session$ns



    output$transform <- shiny::renderUI(
      shiny::tagList(
        shiny::tags$h4("Transform Panel"),
        shiny::tags$p("This functionality allows you to do quick transformations to examine different aspects
        of your panel data.  Transformations can utilize any variables in your data set, though you should",
                      shiny::strong("modify"), "the variables used in the plot, and not create new variables.
                      This is really meant to examine (potentially conditional) transformations of the plotted variables.
                      After you click 'transform data' a tree will appear that contains the transformations that you have
                      applied.  To return to a previous state, click on the node related to that state.  Any further
                      transformations will appear as a new branch off of that node.  Finally, once you have
                      used a function to do a transformation, you should not edit that function.  If you would like to use
                      a similar function, create a new one.
                      "),
        newTransformFunctionUI(ns("tform")),
#        shiny::selectizeInput(ns('trnsfm'), label=NULL, choices=character(),
#                              options = list(create = TRUE, placeholder="Select a previously defined transform function"),
#                              multiple=FALSE),
        transformbuttonUI(ns("transformselectize")),
        shiny::actionButton(ns("edit_tform"), "Edit"),

        shiny::actionButton(ns("transform"), "transform data"),

        transformTrackerUI(ns("tformtrack")),
        shiny::hr()
      )
    )

    newTransformFunctionServer("tform", pipeline_variables)
    transform_fn <- transformbuttonServer("transformselectize", pipeline_variables)


    shiny::observeEvent(input$transform, {

      if(!isTruthy(transform_fn())) {#check if selectize is null
        # fn_txt <- paste('f_compare <- function(panel_data) {', input$filterval, '}', sep='')
        #
        # fn_label <- input$functionValueName
        # pipeline_variables$filter_value_functions <- pipeline_variables$filter_value_functions %>% dplyr::add_row(name=fn_label, fn=input$filterval)
        errorhandle <- "need to select a function!"
        shinyalert::shinyalert(paste0("Oops!", "Code didn't run.  Make sure to select a function first!"), type = "error")
      } else{

        fn_txt_tmp <- pipeline_variables$transform_functions %>% dplyr::filter(.data$name==transform_fn()) %>% dplyr::select(.data$fn)
        fn_snippet <- fn_txt_tmp$fn
        fn_txt <- paste('f_transform <- function(panel_data) {', fn_txt_tmp$fn, '}', sep='')




      #print(pipeline_variables$tree)
      #print(pipeline_variables$current_anchor)

      errorhandle <- NULL
      tryCatch(eval(parse(text = fn_txt)), error = function(e) {errorhandle <<- "there might be an issue with your parsing"})

      tmp_try <- NULL

      if(is.null(errorhandle)){

        tryCatch(tmp_try <- transform_data(pipeline_variables$df_main, f_transform),
                 error = function(e) {errorhandle <<- "your code is throwing an error"},
                 warning=function(w) {errorhandle <<- "throwing some warnings here..."})

      if(is.null(errorhandle)){



        #enter into the tree of actions.  Need to check if currently there.  i.e. check if fn_label exists in tree with current_anchor as parent.

        #check if this transform is already in the tree:
        tmpcheck <- pipeline_variables$tree %>% dplyr::filter(.data$name==transform_fn(), .data$parentId==pipeline_variables$current_anchor, .data$type=="transform")
        if(nrow(tmpcheck)==0){
          max_tree <- max(pipeline_variables$tree$id)
          pipeline_variables$tree <- pipeline_variables$tree %>% dplyr::add_row(id = max_tree+1, parentId = pipeline_variables$current_anchor, name=transform_fn(), type="transform", expression1=fn_snippet)

          pipeline_variables$current_anchor <- max_tree+1
        } else {
          pipeline_variables$current_anchor <- tmpcheck$id
        }

      pipeline_variables$df_main <- tmp_try
      gargoyle::trigger("push_to_tree")


      #shiny::updateCheckboxInput(session, "newtransformfunction", value=FALSE)
      pipeline_variables$shift_data()
      pipeline_variables$trigger_newdat()
      gargoyle::trigger("send_to_panel_plot")

      } else {
        shinyalert::shinyalert(paste0("Oops!", "Code didn't run.  Perhaps there is a typo in your function?  Maybe this is a hint: ",errorhandle, ". Select the function and click 'edit' to edit the function."), type = "error")
      }
      }else{
        shinyalert::shinyalert(paste0("Oops!", "Code didn't run.  Perhaps there is a typo in your function?  Maybe this is a hint: ",errorhandle, ". Select the function and click 'edit' to edit the function."), type = "error")
      }
      }

    })



    shiny::observeEvent(input$edit_tform, {

      shinyalert::shinyalert(html = TRUE,
                             closeOnEsc = TRUE,
                             closeOnClickOutside = TRUE,
                             showConfirmButton = FALSE,text = tagList(
                               shinyjs::disabled(textInput(ns("editTransformName"), label=NULL, value=transform_fn())),
                               textAreaInput(ns("edittransformval"),
                                             label="function(panel_data) {",
                                             value=pipeline_variables$transform_functions %>%
                                               dplyr::filter(.data$name==transform_fn()) %>% dplyr::select(.data$fn),
                                             width="100%", height="300px"), strong("}"),
                               shiny::br(),
                               shiny::actionButton(ns("submitedtransformfunction"), "confirm edit")
                             ))

    }, ignoreInit = TRUE)

    shiny::observeEvent(input$submitedtransformfunction, {


      fn_label <- input$editTransformName

      pipeline_variables$transform_functions <- pipeline_variables$transform_functions %>% dplyr::mutate(fn=base::replace(.data$fn, .data$name==fn_label, input$edittransformval))


      shiny::updateTextInput(session, "editTransformName", value=character())
      shiny::updateTextAreaInput(session, "editTransformval", value=character())


      gargoyle::trigger("transform_selectize_update")

    })




    # shiny::observe({
    #
    #   gargoyle::watch("transform_selectize_update")
    #   shiny::updateSelectizeInput(session, inputId = "trnsfm", choices=pipeline_variables$transform_functions$name, selected=character())
    #
    #
    # })

    shiny::observeEvent(input$returnToRaw, {
      pipeline_variables$df_main <- pipeline_variables$backup_df
      gargoyle::trigger("send_to_panel_plot")
    })

    shiny::observe({
      gargoyle::watch("push_to_tree")
      session$sendCustomMessage(type = "data_tree",
                                message = list(data = jsonlite::toJSON(pipeline_variables$tree),
                                               current_anchor = pipeline_variables$current_anchor))

    })

    shiny::observe({
      gargoyle::watch("filter_chain")

       ancestors <- pipeline_variables$ancestors()

      for(i in 1:length(ancestors)){
        if(ancestors[i]==1){
          pipeline_variables$df_main <- pipeline_variables$backup_df
        }else{
          #print(ancestors[i])
          #print(pipeline_variables$tree)
          fn_txt_tmp <- pipeline_variables$tree %>% dplyr::filter(id==ancestors[i]) %>% dplyr::select(.data$expression1)
          fn_txt <- paste('f_transform <- function(panel_data) {', fn_txt_tmp$expression1, '}', sep='')
          eval(parse(text = fn_txt))

          pipeline_variables$df_main <- transform_data(pipeline_variables$df_main, f_transform)
        }
      }
       pipeline_variables$shift_data()
       pipeline_variables$trigger_newdat()
       #gargoyle::trigger("send_to_panel_plot")
       #gargoyle::trigger("update_variables") #causes infinite loop
       session$sendCustomMessage(type = "data",
                                 message = list(lines = jsonlite::toJSON(pipeline_variables$newdat),
                                                centers = jsonlite::toJSON(pipeline_variables$centers)))

     })

    transformTrackerServer("tformtrack", pipeline_variables)
  })
}

