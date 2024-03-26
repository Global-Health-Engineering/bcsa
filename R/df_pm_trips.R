#' Data on times when open waste burning was observed during the personal
#' monitoring
#'
#' Data on the occurance of burning events during the personal monitoring in
#' four informal settlements. Transect walks were conducted for this.
#'
#' @format A tibble with 81 rows and 11 variables:
#' \describe{
#'   \item{serial_number}{serial number of the MA200 monitoring from which the data is collected}
#'   \item{session_id}{session number of the MA200 monitor (each monitoring session is automatically given a number in the output file of MA200 monitoring data)}
#'   \item{date}{date of monitoring}
#'   \item{time}{time when a burning event was observed}
#'   \item{id}{id is a unique identifier given to every monitoring session and experiment given in the data structure}
#'   \item{date_start}{starting date of the experiment}
#'   \item{date_end}{ending date of the experiment}
#'   \item{start_time}{starting time of the experiment}
#'   \item{end_time}{ending time of the experiment}
#'   \item{exp_type}{type of experiment}
#'   \item{settlement_id}{settlement name at which the monitoring was conducted}
#' }
"df_pm_trips"
