#' Standard column names for weather data to calculate FFDI
#'
#' A named list used as the default for the \code{fields} argument of function
#' \code{\link{calculate_ffdi}}, and used to map input data column names to
#' standard variable names used within the function. For each element in the list,
#' the value is the column name and the element name is the variable.
#'
#' @format A named list with the following names and default values:
#' \describe{
#'   \item{station}{weather station ID number: 'station'}
#'   \item{date}{calendar date: 'date_local'}
#'   \item{hour}{hour (0-23): 'hour_local'}
#'   \item{minute}{minute (0-59): 'min_local'}
#'   \item{precipitation}{precipitation (millimetres): 'precipitation'}
#'   \item{temperature}{temperature (degrees celcius): 'temperature'}
#'   \item{relhumidity}{relative humidity (percent): 'relhumidity'}
#'   \item{windspeed}{wind speed (km per hour): 'windspeed'}
#' }
"cermbStandardFFDIVars"
