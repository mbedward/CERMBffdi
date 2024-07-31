#' Lookup table of input weather data columns required to calculate FFDI
#'
#' @format A two-column data frame:
#' \describe{
#'   \item{varname}{(character) column name to be used with input data}
#'   \item{mandatory}{(logical) \code{TRUE} for mandatory variables;
#'     \code{FALSE} for variables only required to extend an existing time series of FFDI values}
#' }
"FFDIVars"
