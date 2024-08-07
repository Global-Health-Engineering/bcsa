#' Mobile monitoring data in eight settlements
#'
#' Raw data on spatial distribution of light absorbing carbon concentrations in
#' eight settlements, obtained by mobile monitoring. Monitoring was conducted
#' using the mobile vehicle, where the monitor was placed at the front window
#' of the car.
#'
#' @format A tibble with 3540 rows and 25 variables:
#' \describe{
#'   \item{serial_number}{serial number of the MA200 monitoring from which the data is collected}
#'   \item{session_id}{session number of the MA200 monitor (each monitoring session is automatically given a number in the output file of MA200 monitoring data)}
#'   \item{date}{date of monitoring}
#'   \item{time}{time of monitoring}
#'   \item{lat}{latitude of location of monitoring}
#'   \item{long}{longitude of location of monitoring}
#'   \item{uv_bcc}{concentration of black carbon at the UV (ultravoilet) wavelength channel in ng/m3}
#'   \item{blue_bcc}{concentration of black carbon at the Blue wavelength channel in ng/m3}
#'   \item{ir_bcc}{concentration of black carbon at the IR (infrared) wavelength channel in ng/m3}
#'   \item{uv_babs}{absorption coefficient of black carbon at the UV (ultravoilet) wavelength channel}
#'   \item{blue_babs}{absorption coefficient of black carbon at the Blue wavelength channel}
#'   \item{ir_babs}{absorption coefficient of black carbon at the IR (infrared) wavelength channel}
#'   \item{date_time}{date and time of monitoring}
#'   \item{day_type}{type of day of monitoring - weekend or weeday }
#'   \item{id}{id is a unique identifier given to every monitoring session and experiment given in the data structure}
#'   \item{date_start}{starting date of the experiment}
#'   \item{date_end}{end date of the experiment}
#'   \item{exp_type}{type of experiment}
#'   \item{settlement_id}{settlement name at which the monitoring was conducted}
#'   \item{time_of_day}{the days are divided into three types - morning, first half and second half}
#'   \item{type_of_settlement}{type of settlement - formal or informal}
#'   \item{start_time}{starting time of the experiment}
#'   \item{end_time}{ending time of the experiment}
#' }
"df_mm"
