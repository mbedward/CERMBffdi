## List of standard column names for weather data input to
## the `calculate_ffdi` function.

cermbStandardFFDIVars <- list(
  station = 'station',
  date = 'date_local',
  hour = 'hour_local',
  minute = 'min_local',
  precipitation = 'precipitation',
  temperature = 'temperature',
  relhumidity = 'relhumidity',
  windspeed = 'windspeed'
)

usethis::use_data(cermbStandardFFDIVars, overwrite = TRUE)

rm(cermbStandardFFDIVars)
