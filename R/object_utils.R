#' transforms data to grid formation for plotting
#'
#' @param data data requiring transformation
#'
#' @param x variable mapped to x axis
#' @param y variable mapped to y axis
#' @param key variable associated with the panel
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
get_plot_vals <- function(data, x, y, key){
  #changing this to only return normalized data that will be sent once and done.
  panel_string <- NULL
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  key <- rlang::enquo(key)

  get_min_max <- function(data, col) {
    dat <- rlang::eval_tidy(col, data)
    c(min(dat), max(dat))
  }

  xlims <- get_min_max(data, x)
  ylims <- get_min_max(data, y)

  .x <- paste0(".", rlang::as_name(x)) #here and in last 3 replaced quo with as
  .y <- paste0(".", rlang::as_name(y))
  .key <- rlang::as_name(key)


  data <- data %>%
    dplyr::mutate(!!.x := normalise(as.numeric(!!x),
                                    xmin = min_na(as.numeric(!!x)),
                                    xmax = max_na(as.numeric(!!x))))

  data <- data %>%  #for now, assume 1 y variable under consideration, but see earo for modifying
    dplyr::mutate(!!.y := normalise(as.numeric(!!y),
                                    xmin = min_na(as.numeric(!!y)),
                                    xmax = max_na(as.numeric(!!y))))

  data <- data %>% dplyr::mutate(dotx = !!rlang::sym(.x), doty = !!rlang::sym(.y))%>%
    dplyr::mutate(panel_string = !!key) %>% dplyr::mutate(panel_string=as.character(panel_string)) #make sure the panel string works.

  data

}

#' Establishes axes locations for panels and selection rectangles
#'
#' @param x list of panels
#' @param maxcol maximum number of columns for panel grid plot
#' @param width panel dimension
#' @param height panel dimension
#' @param setup_grid pre existing or user-supplied grid
#' @param force_grid is there a specific format for the grid (e.g. number of columns)
#'
#'
get_the_grid <- function(x, maxcol=30, width=.8, height=.8, setup_grid = NA, force_grid = FALSE){

  #for now, x can either be the long list of panels repeated, or just a unique list
  if(length(setup_grid) !=3){
    setup_grid <- setup_grid_groups(x, maxcol, force_grid)
  }



  grids <- assign_grids(
    max(setup_grid$ROW),
    max(setup_grid$COL),
    width = width,
    height = height
  )

  #print(grids)

  rgrid <- setup_grid %>%
    dplyr::left_join(grids, by = c("COL", "ROW")) %>% dplyr::mutate(dotcx = .data$.cx, dotcy=.data$.cy)
  # save(rgrid, file="tmp.rda")
  rgrid
}

get_new_plot_vals <- function(olddata, scoresdata){
  #print(olddata)
  olddata %>% dplyr::select(-c(.data$use_pos, .data$use_score)) %>% dplyr::left_join(scoresdata, by="panel_string") %>%
    dplyr::mutate(dotcx = (.data$scorex-min(.data$scorex))/(max(.data$scorex)-min(.data$scorex)),
           dotcy = (.data$scorey-min(.data$scorey))/(max(.data$scorey)-min(.data$scorey)))
}


#' Obtain centers of panels
#'
#' @param data panel data
#' @param scores not needed?
#' @param width_factor width multiplier
#' @param height_factor height multiplier
#' @param width width of panel
#' @param height height of panel
#'
get_centers_with_bounds <- function(data, scores,
                                    width_factor = 1, height_factor=1, width=NULL, height=NULL){
  .x1 <- NULL
  .x2 <- NULL
  .y1 <- NULL
  .y2 <- NULL
  if(is.null(width)){
    width <- ggplot2::resolution(data$.cx, zero = FALSE) * width_factor
  }
  if(is.null(height)){
    height <- ggplot2::resolution(data$.cy, zero = FALSE) * height_factor
  }


  data %>% dplyr::select(.data$ROW, .data$COL, .data$PANEL, .data$.cx, .data$.cy) %>% dplyr::distinct() %>%
    dplyr::mutate(.x1=.data$.cx, .y1=.data$.cy,
           .x2=.data$.cx+width, .y2=.data$.cy+height, width=width, height=height) %>%
    dplyr::mutate(dotx1 = .x1, dotx2=.x2, doty1=.y1, doty2=.y2, dotcx=.data$.cx, dotcy=.data$.cy) %>%
    dplyr::mutate(panel_string = as.character(.data$PANEL))


}

#' Center distance function for panel scatterplot
#'
#' @param centers center of panels
#' @param width width of panels
#' @param height height of panels
#'
get_centers_dist <- function(centers, width, height){
  new_cx <- NULL
  new_cy <- NULL
  pstring <- centers %>% dplyr::select(.data$panel_string)
  new_cent <- centers %>% dplyr::mutate(new_cx = .data$dotcx + width/2, new_cy = .data$dotcy + height/2) %>% dplyr::select(new_cx, new_cy)
  xdist <- (as.matrix(stats::dist(new_cent$new_cx, upper=TRUE))<width)*1
  ydist <- (as.matrix(stats::dist(new_cent$new_cy, upper=TRUE))<height)*1
  tibble::tibble(panel_string = as.vector(pstring$panel_string), num_overlap = rowSums(xdist*ydist))
}


#copied with modification from https://github.com/earowang/sugrrants/blob/master/R/frame-calendar.R



#' Assign panels to the grid
#'
#' @param ROW number of rows
#' @param COL number of columns
#' @param width width of panel
#' @param height height of panel
#'
assign_grids <- function(ROW, COL, width, height) {

  col_grids <- seq(1, 0, length.out = ROW)
  row_grids <- seq(0, 1, length.out = COL)
  grids <- expand.grid2(.gx = row_grids, .gy = col_grids)
  combs <- expand.grid2(COL = seq_len(COL), ROW = seq_len(ROW))
  out <- cbind(combs, grids)

  if(COL>1){
    min_x <- min_diff(row_grids)
  }
  if(ROW>1){
    min_y <- min_diff(col_grids)
  }

  if(COL>1){
    out$.cx <- out$.gx + (min_x - (width / (COL - 1))) / 2
  }else{
    out$.cx <- out$.gx
  }
  if(ROW>1){
    out$.cy <- out$.gy + (min_y - (height / (ROW - 1))) / 2
  }else{
    out$.cy <- out$.gy
  }


  width <- ggplot2::resolution(out$.gx, zero = FALSE) * width
  height <- ggplot2::resolution(out$.gy, zero = FALSE) * height

  out$width <- width
  out$height <- height

  out
}

#from utils
#' wrapper for expand grid
#'
#' @param ... variables passed to expand grid
#'
expand.grid2 <- function(...) {
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

#' minimum difference between successive entries in a vector
#'
#' @param x vector
#'
min_diff <- function(x) {
  min(abs(diff(x, na.rm = TRUE)))
}

#' wrapper for min where na.rm is TRUE
#'
#' @param ... parameters passed to min
#'
min_na <- function(...) {
  min(..., na.rm = TRUE)
}

#' wrapper for max where na.rm is TRUE
#'
#' @param ... parameters passed to max
#'
max_na <- function(...) {
  max(..., na.rm = TRUE)
}

#' Test if vector is constant
#'
#' @param x vector
#'
is_constant <- function(x) {
  diff(range(x, na.rm = TRUE)) < .Machine$double.eps ^ 0.5
}

#' function to normalize a vector
#'
#' @param x data vector
#' @param xmin min supplied if min should not be derived from the vector
#' @param xmax max supplied if max should not be derived from the vector
#'
normalise <- function(x, xmin = NULL, xmax = NULL) {
  if (is_constant(x)) return(x)
  if (is.null(xmin)) xmin <- min_na(x)
  if (is.null(xmax)) xmax <- max_na(x)
  (x - xmin) / (xmax - xmin)
}



#### my own utils
#' Place panels in grid
#'
#' @param x vector of panels
#' @param maxcol maximal columns allowed
#' @param force_grid TRUE if number of columns should not be derived from the number of panels
#'
setup_grid_groups <- function(x, maxcol, force_grid = FALSE){

  #x says which panel a datapoint belongs to
  x <- unique(x)
  numx <- length(x)
  #for now, assume that you don't go over the max number, but need to handle this eventually.
  #probably will create "lists" of frames with essentially a recursive call to setup_grid_groups.

  #try to get as close to square as possible... user can modify by fussing around with max numbers

  if(force_grid){
    numcol <- maxcol
  }else{
  numcol <- min(ceiling(sqrt(numx)), maxcol)
  }
  numrow <- ceiling(numx/numcol)


  colind <- rep_len(seq_len(numcol), numx)
  rowind <- rep(seq_len(numrow), each = numcol, length.out = numx)

  seq_x <- seq_len(numx)

  tibble::tibble(ROW = rowind, COL = colind, panel_string = x)
}

#grab comparison dataset (keep original in there)
#' Function to calculate comparison scores
#'
#' @param df data
#' @param panel panel chosen for comparison
#' @param fn function to perform comparison
#'
#' @return data frame with scores
#'
get_comparison_scores <- function(df, panel, fn){
  data <- NULL
  score <- NULL
  tmp_df <- df %>% dplyr::group_by(.data$panel_string) %>% tidyr::nest()
  comp_df <- df %>% dplyr::filter(.data$panel_string %in% panel) #%>% rename(x_selected = x, y_selected = y)
  wrapped_fn <- function(panel_selected){
    function(panel_data){
      fn(panel_data, panel_selected)
    }
  }

  tmp_fn <- wrapped_fn(comp_df)

  tmp_df %>% dplyr::mutate(score=purrr::map(data, tmp_fn)) %>% tidyr::unnest(cols=c(score)) %>% dplyr::select(score, .data$panel_string)
}

#' Compute scores from user supplied function
#'
#' @param data data set
#' @param fn1 function to calculate score
#' @param fn2 optional function if calculating scores for scatterplot
#'
#' @return data frame with scores
#'
get_value_scores <- function(data, fn1, fn2=NULL){
  score <- NULL
  scorex <- NULL
  scorey <- NULL
  tmp_data <- data %>% dplyr::group_by(.data$panel_string) %>% tidyr::nest()
  if(is.null(fn2)){
    tmp_data %>% dplyr::mutate(score=purrr::map(data, fn1)) %>% tidyr::unnest(cols=score)
  } else {
    tmp_data %>% dplyr::mutate(scorex=purrr::map(data, fn1), scorey=purrr::map(data, fn2)) %>% tidyr::unnest(cols=c(scorex, scorey))
  }
}



#' Function to transform data according to user specified function
#'
#' @param data dataset
#' @param transformfn transform function
#'
#' @return tibble with transformed data
#'
transform_data <- function(data, transformfn){
  newdat <- NULL
  tmp_data <- data %>% dplyr::group_by(.data$panel_string) %>% tidyr::nest()
  tmp_data %>% dplyr::mutate(newdat=purrr::map(data, transformfn)) %>% dplyr::select(-data) %>% tidyr::unnest(cols=newdat) %>%
    dplyr::ungroup()
}


#' Function that returns identity of the parent node
#'
#' @param tree transform tree
#' @param node current node
#'
#' @return parent node
#'
get_parent_chain <- function(tree, node){
  involved_parents <- c(node)
  while(node>1){
    node <- tree %>% dplyr::filter(.data$id==node) %>% dplyr::select(.data$parentId) %>% as.integer()
    involved_parents <- c(node, involved_parents)
  }
  involved_parents
}

#' Returns a series of transformations to get to current node
#'
#' @param tree transform tree
#' @param parent_chain chain of nodes leading to current node
#'
#' @return series of transform functions
#'
get_reconstruction_chain <- function(tree, parent_chain){
  tree %>% dplyr::filter(.data$id %in% parent_chain) %>% dplyr::arrange(.data$id)
}

#' Takes series of transformations and applies them to the data
#'
#' @param tree transformations
#' @param data data to be transformed
#'
#' @return transformed data
#'
apply_reconstruction <- function(tree, data){
  f_transform <- NULL
  if(nrow(tree)==1) {
    data
  } else{
    tree <- tree %>% dplyr::filter(.data$id>1)
    for(i in 1:nrow(tree)){
      tmp_branch <- tree[i,]
      if(tmp_branch$type=="transform"){
        fn_txt_tmp <- tmp_branch$expression1
        eval(parse(text = fn_txt_tmp))
        data <- transform_data(data, f_transform)
      }
    }
  }

  data
}

#' Obtain series of ancestors to current node
#'
#' @param index current node index
#' @param data dataset
#'
#' @return vector of ancestors
#'
get_ancestors <- function(index, data){
  if(index==1){
    1
  }else{
    parentid <- data %>% dplyr::filter(.data$id==index) %>% dplyr::select(.data$parentId) %>% as.numeric()
    c(get_ancestors(parentid, data), index)
  }
}
