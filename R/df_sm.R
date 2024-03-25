#' Raw data from stationary monitoring in two settlements
#'
#' Data on ambient concentrations of light absorbing carbon during the months of
#' July-August 2023. The data was collected from one formal and one informal
#' settlement.
#'
#' @format A tibble with 20756 rows and 21 variables:
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
#'   \item{start_time}{starting time of the experiment}
#'   \item{end_time}{end time of the experiment}
#'   \item{exp_type}{type of experiment}
#'   \item{settlement_id}{settlement name at which the monitoring was conducted}
#' }
