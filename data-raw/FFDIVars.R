## Lookup table of input data columns required by the `calculate_ffdi` function.

FFDIVars <- data.frame(
  varname = c('station', 'date', 'hour', 'minute',
              'precipitation', 'temperature', 'relhumidity', 'windspeed',
              'tmaxdaily', 'precipdaily', 'kbdi', 'drought', 'ffdi'),

  mandatory = c(TRUE, TRUE, TRUE, TRUE,
                TRUE, TRUE, TRUE, TRUE,
                FALSE, FALSE, FALSE, FALSE, FALSE)
)

usethis::use_data(FFDIVars, overwrite = TRUE)

rm(FFDIVars)
