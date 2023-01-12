#' Simulated data used to generate figures in accompanying manuscript
#'
#' Data intended to show specific issues with currently available visualization tools
#'
#' @format ## `example_data_from_paper`
#' A data frame with 5,100 rows and 6 columns:
#' \describe{
#'   \item{seconds}{x variable (e.g. time)}
#'   \item{series}{time series to which the data belong}
#'   \item{identity}{group to which the time series belongs}
#'   \item{y}{y value}
#'   \item{identity_16, identity_random}{group to which time series belongs under slight corruption and random permutation}
#'   ...
#' }
#' @source simulated
"example_data_from_paper"

#' Simulated data with no row or column information
#'
#' simulated from a normal distribution-- no signal
#'
#' @format ## `example_data_no_row_col`
#' A data frame with 20,000 rows and 3 columns:
#' \describe{
#'   \item{x}{x variable (e.g. time)}
#'   \item{y}{y value}
#'   \item{z}{panel identifier}
#'   ...
#' }
#' @source simulated
"example_data_no_row_col"

#' Simulated data with row and column information
#'
#' simulated from a normal distribution-- no signal
#'
#' @format ## `example_data_with_row_col`
#' A data frame with 20,000 rows and 3 columns:
#' \describe{
#'   \item{x}{x variable (e.g. time)}
#'   \item{y}{y value}
#'   \item{yprime}{alternative y value}
#'   \item{z}{panel identifier}
#'   \item{rows}{row location for panel}
#'   \item{cols}{column location for panel}
#'   ...
#' }
#' @source simulated
"example_data_with_row_col"
