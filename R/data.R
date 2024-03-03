#' Simulated data used to generate figures in accompanying manuscript
#'
#' Data intended to show specific issues with currently available
#'    visualization tools
#'
#' @format ## `example_data_from_paper`
#' A data frame with 5,100 rows and 6 columns:
#' \describe{
#'   \item{seconds}{x variable (e.g. time)}
#'   \item{series}{time series to which the data belong}
#'   \item{identity}{group to which the time series belongs}
#'   \item{y}{y value}
#'   \item{identity_16, identity_random}{group to which time series belongs
#'        under slight corruption and random permutation}
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

#' Simulated data with row and column information
#'
#' simulated from a normal distribution-- no signal
#'
#' @format ## `running_data`
#' A data frame with 194314 rows and 48 columns:
#' \describe{
#'   \item{run_id}{}
#'   \item{limb}{}
#'   \item{timestamp}{}
#'   \item{step}{}
#'   \item{stide_pace}{}
#'   \item{step_rate}{}
#'   \item{stride_length}{}
#'   \item{contact_time}{}
#'   \item{flight_ratio}{}
#'   \item{power}{}
#'   \item{shock}{}
#'   \item{impact_gs}{}
#'   \item{braking_gs}{}
#'   \item{footstrike_type}{}
#'   \item{pronation_excursion_fs_mp}{}
#'   \item{max_pronation_velocity}{}
#'   \item{grf_vert_peak}{}
#'   \item{contact_ratio}{}
#'   \item{flight_time}{}
#'   \item{stride_angle}{}
#'   \item{kleg}{}
#'   \item{kvert}{}
#'   \item{grf_vert}{}
#'   \item{grf_horiz}{}
#'   \item{swing_force_rate}{}
#'   \item{total_force_rate}{}
#'   \item{time_max_fs}{}
#'   \item{time_fs_mpv}{}
#'   \item{time_mpv_mp}{}
#'   \item{time_mp_to}{}
#'   \item{time_to_min}{}
#'   \item{time_swing}{}
#'   \item{medial_lateral_gs}{}
#'   \item{braking_gs_amp}{}
#'   \item{impact_gs_amp}{}
#'   \item{vertical_speed}{}
#'   \item{elevation_gain}{}
#'   \item{swing_excursion}{}
#'   \item{yaw_excursion_fs_mp}{}
#'   \item{yaw_excursion_mp_to}{}
#'   \item{yaw_excursion_swing}{}
#'   \item{stance_velocity_max_fs_mp}{}
#'   \item{stance_velocity_max_mp_to}{}
#'   \item{temperature}{}
#'   \item{step_length}{}
#'   \item{pronation_excursion_mp_to}{}
#'   \item{stance_excursion_fs_mp}{}
#'   \item{stance_excursion_mp_to}{}
#'   ...
#' }
#' @source personal collection from experienced funner
"running_data"
