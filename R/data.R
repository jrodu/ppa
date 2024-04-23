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
#'   \item{run_id}{unique id for run}
#'   \item{limb}{variable indicating leg sensor is placed on ('l' or 'r')}
#'   \item{timestamp}{time in ms}
#'   \item{step}{sequential step number in run}
#'   \item{stide_pace}{Runscribe variable derived from primary variables}
#'   \item{step_rate}{steps per minute}
#'   \item{stride_length}{distance (m) between successive placements of same foot}
#'   \item{contact_time}{time (ms) foot is in contact with ground from initial contact to toe-off}
#'   \item{flight_ratio}{Runscribe variable derived from primary variables}
#'   \item{power}{Runscribe variable derived from primary variables}
#'   \item{shock}{Runscribe variable derived from primary variables}
#'   \item{impact_gs}{vertical component of change in acceleration of the foot at initial contact (how hard you are hitting the ground)}
#'   \item{braking_gs}{horizontal component of change in acceleration of the foot at initial contact (forces slowing you down with each step)}
#'   \item{footstrike_type}{portion of foot that first contacts ground (1-5=rearfoot, 6-10=midfoot, and 11-16=forefoot)}
#'   \item{pronation_excursion_fs_mp}{pronation range of motion from initial contact to maximum pronation (how much foot turns over)}
#'   \item{max_pronation_velocity}{rate of pronation over time from initial contact to maximum pronation (how quickly foot turns over)}
#'   \item{grf_vert_peak}{Runscribe variable derived from primary variables}
#'   \item{contact_ratio}{Runscribe variable derived from primary variables}
#'   \item{flight_time}{Runscribe variable derived from primary variables}
#'   \item{stride_angle}{Runscribe variable derived from primary variables}
#'   \item{kleg}{Runscribe variable derived from primary variables}
#'   \item{kvert}{Runscribe variable derived from primary variables}
#'   \item{grf_vert}{Runscribe variable derived from primary variables}
#'   \item{grf_horiz}{Runscribe variable derived from primary variables}
#'   \item{swing_force_rate}{Runscribe variable derived from primary variables}
#'   \item{total_force_rate}{Runscribe variable derived from primary variables}
#'   \item{time_max_fs}{Runscribe variable derived from primary variables}
#'   \item{time_fs_mpv}{Runscribe variable derived from primary variables}
#'   \item{time_mpv_mp}{Runscribe variable derived from primary variables}
#'   \item{time_mp_to}{Runscribe variable derived from primary variables}
#'   \item{time_to_min}{Runscribe variable derived from primary variables}
#'   \item{time_swing}{Runscribe variable derived from primary variables}
#'   \item{medial_lateral_gs}{Runscribe variable derived from primary variables}
#'   \item{braking_gs_amp}{Runscribe variable derived from primary variables}
#'   \item{impact_gs_amp}{Runscribe variable derived from primary variables}
#'   \item{vertical_speed}{Runscribe variable derived from primary variables}
#'   \item{elevation_gain}{Runscribe variable derived from primary variables}
#'   \item{swing_excursion}{Runscribe variable derived from primary variables}
#'   \item{yaw_excursion_fs_mp}{Runscribe variable derived from primary variables}
#'   \item{yaw_excursion_mp_to}{Runscribe variable derived from primary variables}
#'   \item{yaw_excursion_swing}{Runscribe variable derived from primary variables}
#'   \item{stance_velocity_max_fs_mp}{Runscribe variable derived from primary variables}
#'   \item{stance_velocity_max_mp_to}{Runscribe variable derived from primary variables}
#'   \item{temperature}{Runscribe variable derived from primary variables}
#'   \item{step_length}{Runscribe variable derived from primary variables}
#'   \item{pronation_excursion_mp_to}{Runscribe variable derived from primary variables}
#'   \item{stance_excursion_fs_mp}{Runscribe variable derived from primary variables}
#'   \item{stance_excursion_mp_to}{Runscribe variable derived from primary variables}
#'   ...
#' }
#' @source personal collection from experienced funner
"running_data"
