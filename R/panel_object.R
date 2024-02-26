#' @import R6

PanelObject <- R6::R6Class("PanelObject",
                         public = list(
                           load_data = NULL,
                           original_data = NULL,
                           panel_data_type = NULL,
                           labels = NULL,
                           tree = tibble::tibble(
                             id=numeric(), parentId=numeric(),
                             name=character(), type=character(),
                             expression1=character(), expression2=character()),
                           current_anchor=NULL,
                           selected_state=NULL,
                           preselected = NULL,
                           cur_selection = NULL,
                           # select_by_col = FALSE,
                           # select_by_row = FALSE,
                           invert_selection = 0,
                           var1=NULL,
                           var2=NULL,
                           panel_name=NULL,
                           plot_type="line",
                           filter_selection_functions = tibble::tibble(
                             name=character(), fn=character()),
                           filter_value_functions = tibble::tibble(
                             name=character(), fn=character()),
                           transform_functions = tibble::tibble(
                             name=character(), fn=character()),
                           hide_panels = NULL,
                           isolate_panels = NULL,
                           filtereddf = NULL,
                           backup_df = NULL,
                           df_main = NULL,
                           df_use = NULL,
                           newdat = NULL,
                           centers = NULL,
                           setup_grid = NULL,
                           randomize = "panel",
                           force_grid = FALSE,
                           maxcol = 40,
                           height_ratio = 1,
                           newdatacomein = function(
                              var1 = NA, var2 = NA, panel_name = NA){
                             self$original_data <- tibble::as_tibble(
                               self$load_data)
                             self$var1 <- rlang::enquo(var1)
                             self$var2 <- rlang::enquo(var2)
                             self$panel_name <- rlang::enquo(panel_name)
                             self$backup_df <- self$original_data %>%
                               dplyr::mutate(
                                 panel_string = !!self$panel_name) %>%
                               dplyr::mutate(
                                 panel_string = as.character(panel_string)) %>%
                               dplyr::group_by(panel_string) %>%
                               dplyr::mutate(
                                 pointid=as.character(dplyr::row_number())) %>%
                               dplyr::ungroup()
                             self$df_main <- self$backup_df
                             self$df_use <- self$df_main
                             self$define_filtered()
                             self$trigger_newdat()

                             self$labels <- tibble::tibble(
                               panel_string = unique(self$newdat$panel_string))

                             if(1 %in% self$tree$id){
                               self$tree <- self$tree
                             }else{
                               self$tree <- self$tree %>%
                                 dplyr::add_row(id = 1,
                                                name="initial data",
                                                type="node")
                             }
                             self$current_anchor <- 1
                           },
                           trigger_newdat = function() {
                             #update plotting data
                             self$newdat <- get_plot_vals(self$df_use,
                                                          !!self$var1,
                                                          !!self$var2,
                                                          !!self$panel_name)

                             self$centers <- get_the_grid(
                               self$newdat$panel_string,
                               maxcol=self$maxcol,
                               setup_grid = self$setup_grid,
                               force_grid = self$force_grid) %>%
                               dplyr::left_join(
                                 self$filtereddf, by="panel_string") %>%
                              dplyr::filter(
                                !panel_string %in% self$hide_panels,
                                !panel_string %in% self$isolate_panels)
                             if(is.null(self$setup_grid)){
                             self$setup_grid <- self$centers %>%
                               dplyr::select(ROW, COL, panel_string)
                             }

                             invisible(self)
                           },
                           trigger_tmpnewdat = function() {
                             #update plotting data
                             self$newdat <- get_plot_vals(
                               self$df_use, !!self$var1, !!self$var2,
                               !!self$panel_name)
                             self$centers <- get_the_grid(
                               self$newdat$panel_string, maxcol=self$maxcol,
                               setup_grid = NULL) %>%
                               dplyr::left_join(
                                 self$filtereddf, by="panel_string") %>%
                               dplyr::filter(
                                 !panel_string %in% self$hide_panels,
                                 !panel_string %in% self$isolate_panels)

                             invisible(self)
                           },
                           shift_data = function() {
                             #update used data based on panels of interest
                             self$df_use <- self$df_main %>%
                               dplyr::filter(
                                 !panel_string %in% self$hide_panels,
                                 !panel_string %in% self$isolate_panels)
                             invisible(self)
                           },
                           define_filtered = function() {
                             self$filtereddf <- self$df_use %>%
                               dplyr::select(!!self$panel_name) %>%
                               unique() %>%
                               dplyr::mutate(
                                 panel_string = !!self$panel_name) %>%
                               dplyr::mutate(
                                 panel_string = as.character(panel_string)) %>%
                               dplyr::mutate(
                                 score=0, use_score=0, use_pos=0) %>%
                               dplyr::select(-!!self$panel_name)
                             invisible(self)
                           },
                           update_labels = function(label = NA){
                             if(label %in% names(self$labels)){
  self$labels[[label]][self$labels$panel_string %in% self$selected_state] <- 1
                             } else{
                               not_labeled <- setdiff(
                                 self$df_use$panel_string, self$selected_state)
                               tmp_tbble <- tibble::tibble(
                        panel_string = self$selected_state, !!label :=1) %>%
                                 dplyr::add_row(panel_string = not_labeled,
                                                !!label :=0)
                               self$labels <- self$labels %>%
                                 dplyr::left_join(tmp_tbble, by="panel_string")
                             }
                             invisible(self)
                           },
                           reset_filter_criterion = function(){
                             self$cur_selection <- self$selected_state
                             self$invert_selection <- 0
                             invisible(self)
                           },
                           ancestors = function(){
                             get_ancestors(self$current_anchor, self$tree)
                           }

                         ),
                         private=list())
