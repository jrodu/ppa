#' Create grid layout for panels
#'
#' @param dat data being plotted
#' @param rowgroup if applicable, the variable that identifies the row
#'                  for a panel
#' @param colgroup if applicable, the variable that identifies the
#'                  column for a panel
#' @param panelgroup variable that specifies the panel a datapoint belongs to
#' @param randomize whether the grid should be randomized, "none" (default)
#'                  "rows", "cols", "both", or "panels" (breaks association
#'                  with row/col)
#'
#' @return a tibble containing row and column assignment for each panel
#'
get_rowcol <- function(dat, rowgroup, colgroup, panelgroup, randomize="none"){
  COL <- NULL
  ROW <- NULL
  panel_string <- NULL
  if(missing(rowgroup)){
    hasrow <- FALSE
  } else {
    hasrow <- TRUE
    xist <- rlang::enquo(rowgroup)
  }
  if(missing(colgroup)){
    hascol <- FALSE
  } else {
    hascol <- TRUE
    yist <- rlang::enquo(colgroup)
  }

  zist <- rlang::enquo(panelgroup)



  if(hasrow){
    if(hascol){
      getpanel <- dat %>% dplyr::group_by(!!xist) %>%
        dplyr::mutate(ROW=dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!yist) %>%
        dplyr::mutate(COL = dplyr::cur_group_id()) %>%
        dplyr::ungroup()
    } else {
      getpanel <- dat %>% dplyr::group_by(!!xist) %>%
        dplyr::mutate(ROW=dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!xist, !!zist) %>%
        dplyr::mutate(COL = dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!xist) %>%
        dplyr::mutate(COL = COL - (min(COL) - 1)) %>%
        dplyr::ungroup()
    }
  } else {
    if(hascol){
      getpanel <- dat %>%
        dplyr::group_by(!!yist) %>%
        dplyr::mutate(COL = dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!yist, !!zist) %>%
        dplyr::mutate(ROW = dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!yist) %>%
        dplyr::mutate(ROW = ROW - (min(ROW) - 1)) %>%
        dplyr::ungroup()
    }
  }



  panelgrid <- getpanel %>% dplyr::mutate(panel_string = !!zist) %>%
    dplyr::mutate(panel_string = as.character(panel_string)) %>%
    dplyr::select(ROW, COL, panel_string) %>% base::unique()


  if(randomize=="rows"){
    shuffle_row(panelgrid)
  } else if(randomize=="cols"){
    shuffle_col(panelgrid)
  } else if(randomize=="both"){
    shuffle_rowcol(panelgrid)
  } else if(randomize=="panels"){
    shuffle_panel(panelgrid)
  } else {
    panelgrid
  }

}


#' Shuffles placement grid by row
#'
#' @param grid tibble with ROW, COL, and panel
#'
#' @return grid with rows permuted from input grid
#'
#'
#'
shuffle_row <- function(grid){
  maxrow <- base::max(grid$ROW)
  df_map <- base::data.frame(ROW=1:maxrow, toROW = sample(1:maxrow))
  grid %>% dplyr::left_join(df_map, by="ROW") %>%
    dplyr::select(ROW=.data$toROW, .data$COL, .data$panel_string)
}

#' Shuffles placement grid by column
#'
#' @param grid tibble with ROW, COL, and panel
#'
#' @return grid with columns permuted from input grid
#'
#'
#'
shuffle_col <- function(grid){
  maxcol <- base::max(grid$COL)
  df_map <- base::data.frame(COL=1:maxcol, toCOL = sample(1:maxcol))
  grid %>% dplyr::left_join(df_map, by="COL") %>%
    dplyr::select(.data$ROW, COL=.data$toCOL, .data$panel_string)
}

#' Shuffles placement grid by row and column
#' Note that this does not break the dependence of a panel on the row or column
#' i.e. two panels that originally share a row still share a row, and two panels
#' that originally share a column still share a column
#'
#' @param grid tibble with ROW, COL, and panel
#'
#' @return grid with rows and columns permuted from input grid
#'
#'
#'
shuffle_rowcol <- function(grid){
  maxrow <- base::max(grid$ROW)
  maxcol <- base::max(grid$COL)

  df_map <- base::data.frame(ROW=1:maxrow, toROW = sample(1:maxrow))
  grid <- grid %>% dplyr::left_join(df_map, by="ROW") %>%
    dplyr::select(ROW=.data$toROW, .data$COL, .data$panel_string)

  df_map <- base::data.frame(COL=1:maxcol, toCOL = sample(1:maxcol))
  grid %>% dplyr::left_join(df_map, by="COL") %>%
    dplyr::select(.data$ROW, COL=.data$toCOL, .data$panel_string)
}

#' Shuffles placement grid by panel ignoring row and column
#' Note that this function does break dependence on both row and column
#'
#' @param grid tibble with ROW, COL, and panel
#'
#' @return grid with panels shuffled, ignoring row and column assignments.
#'
#'
#'
shuffle_panel <- function(grid){
  gridtmp <- grid %>% dplyr::select(.data$ROW, .data$COL)
  gridtmp <- gridtmp[sample(nrow(gridtmp)),]

  gridtmp$panel_string <- grid$panel_string

  gridtmp
}


#' save variables that will be useful for future analyses
#'
#' @param pipe_vars R6 object containing global variables
#' @param session_name session identifier
#'
#'
#'
save_vars <- function(pipe_vars, session_name){

  varlist <- list()

  if(length(pipe_vars$labels)>1){
    labs <- pipe_vars$labels
    class(labs$panel_string) <- pipe_vars$panel_data_type
    labs <- labs %>% dplyr::rename(!!pipe_vars$panel_name := .data$panel_string)
    varlist$labels <- labs
  }

  if(nrow(pipe_vars$tree)>1) varlist$workflow_tree <- pipe_vars$tree

  if(nrow(pipe_vars$filter_selection_functions)>0){
    varlist$comparison_functions <- pipe_vars$filter_selection_functions}
  if(nrow(pipe_vars$filter_value_functions)>0){
    varlist$score_functions <- pipe_vars$filter_value_functions}
  if(nrow(pipe_vars$transform_functions)>0){
    varlist$transform_functions <- pipe_vars$transform_functions}

  if(!is.null(pipe_vars$hide_panels)){
    varlist$excluded_panels <- pipe_vars$hide_panels
    class(varlist$excluded_panels) <- pipe_vars$panel_data_type
  }

  varlist

}

