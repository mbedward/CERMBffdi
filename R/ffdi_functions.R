#' Calculate FFDI based on weather records for one or more stations
#'
#' This function calculates FFDI (Forest Fire Danger Index) and the related
#' variables: KBDI (Keetch-Bryam Drought Index) and Drought Factor. If there are
#' missing days in the time series, dummy records will be added with all weather
#' variables set to \code{NA}. This allows KBDI, drought factor and FFDI to be
#' calculated for the rest of the time series. FFDI values for the dummy records
#' will be set to \code{NA}. KBDI values (and resulting drought factor values)
#' are set following the protocol described for \code{\link{calculate_kbdi}}. The
#' records for each station must cover a period of at least 21 days for FFDI to
#' be calculated, since Drought Factor requires an initial 20 days of daily
#' rainfall data.
#'
#' @param dat A data frame with records for one or more weather stations
#'   with columns for station number, date, hour, minute, precipitation,
#'   temperature, relative humidity and wind speed. See \code{fields}
#'   argument (below) for how to relate column names to variables.
#'
#' @param fields A named vector or list where names are variables (station,
#'   date, hour, minute, precipitation, temperature, relhumidity, windspeed) and
#'   values are the corresponding column names in the input data frame. The
#'   default is to use the list object \code{cermbStandardFFDIVars} that is
#'   included in the package.
#'
#' @param av.rainfall Average annual rainfall value(s) to use in the calculation
#'   of KBDI (on which FFDI relies). Can either be a single numeric value to be
#'   applied to all stations in the input data, or a two-column matrix or data
#'   frame with station numbers in the first column and average rainfall values
#'   in the second column.
#'
#' @param datatype Either a specified data type ('aws' or 'synoptic') or 'guess'
#'   (default) to guess the type from the data. Case-insensitive and may be
#'   abbreviated. The data type controls how rainfall values are aggregated to
#'   daily values for the calculation of KBDI and drought factor. AWS rainfall
#'   values are total since 9am, whereas Synoptic values are rainfall for the
#'   record time step. If 'guess', the function checks whether there are records
#'   for sub-hourly time steps (AWS data) or not (Synoptic data).
#'
#' @return A data frame with columns:
#'   \code{'station'};
#'   date, hour and minute (named to match input names, e.g. date_local);
#'   \code{'tmaxdaily'} (maximum temperature for calendar day);
#'   \code{'precipdaily'} (total rainfall from 09:01 to 09:00 the next day);
#'   \code{'kbdi'} (Keetch-Bryam drought index);
#'   \code{'drought'} (daily drought factor values);
#'   \code{'ffdi'} (FFDI value based on daily drought factor and time-step
#'     values for temperature, wind speed and relative humidity.
#'
#' @seealso \code{\link{cermbStandardFFDIVars}} for the default list to relate
#'   variables to input column names.
#'
#' @export
#'
calculate_ffdi <- function(dat,
                           fields = cermbStandardFFDIVars,
                           av.rainfall = NULL,
                           datatype = c("guess", "aws", "synoptic")) {

  # Make sure we have a plain vanilla data frame
  dat <- as.data.frame(dat)

  colnames(dat) <- tolower(colnames(dat))

  # tolower() will convert a list to a vector and lose the element
  # names, so get the names first
  nms <- tolower(names(fields))
  fields <- tolower(fields)
  names(fields) <- nms

  # Check names have been provided for all fields
  RequiredFields <- c("station", "date", "hour", "minute",
                      "precipitation", "temperature", "relhumidity",
                      "windspeed")

  ii <- RequiredFields %in% names(fields)
  if (!all(ii)) {
    msg <- paste(names(fields)[!ii], sep = ", ")
    stop("No column name(s) specified for ", msg)
  }

  # Set standard column names and drop any additional columns
  ii <- match(fields, colnames(dat))
  if (any(is.na(ii))) {
    stop("Bummer - problem matching field names to data column names")
  }

  colnames(dat)[ii] <- names(fields)

  dat <- dat[, ii]

  datatype <- match.arg(tolower(datatype), c("guess", "aws", "synoptic"))
  if (datatype == "guess") {
    datatype <- .guess_data_type(dat)
  }


  # Station numbers in data
  stations <- unique(dat$station)
  nstns <- length(stations)


  # Helper to check for (perhaps) valid rainfall values
  is_valid_rain <- function(x) {
    is.numeric(x) & !is.na(x) & !is.infinite(x) & x > 0
  }

  # Average rainfall values for station(s)
  if (is.null(av.rainfall)) {
    stop("av.rainfall (average annual rainfall for stations) must be provided")
  }

  if (is.matrix(av.rainfall) || is.data.frame(av.rainfall)) {
    if (ncol(av.rainfall) != 2) {
      stop("Matrix or data frame for average rainfall should have ",
           "two columns (station, rainfall)")
    }

    if (is.data.frame(av.rainfall)) {
      av.rainfall <- as.matrix(av.rainfall)
    }

  } else {
    # Turn single value into matrix
    av.rainfall <- cbind(station = stations, rainfall = av.rainfall[1])
  }

  # Check average rainfall matrix
  ii <- stations %in% av.rainfall[,1]
  if (!all(ii)) {
    stop("No average rainfall specified for station(s): ", stations[ii])
  }

  if (!all( is_valid_rain(av.rainfall[,2]) )) {
    stop("Invalid average rainfall value(s): ", av.rainfall[ii,2])
  }


  res <- lapply(stations, function(the.stn) {
    ii.dat <- dat$station == the.stn
    ii.rain <- av.rainfall[,1] == the.stn

    .do_calculate_ffdi(dat[ii.dat, ],
                       datatype,
                       av.rainfall[ii.rain, 2])
  })

  dplyr::bind_rows(res)
}


# Private worker function for calculate_ffdi
#
.do_calculate_ffdi <- function(dat.stn,
                               datatype,
                               av.rainfall = NULL) {

  # Sanity checks on the upstream call
  stopifnot(dplyr::n_distinct(dat.stn$station) == 1)

  stopifnot(datatype %in% c("aws", "synoptic"))

  stopifnot(!is.null(av.rainfall))
  stopifnot(length(av.rainfall) == 1 && av.rainfall > 0)

  # Check the time series is long enough for KBDI and drought
  # calculations which need 20 prior days of rainfall data
  dat.stn <- dat.stn %>%
    dplyr::arrange(date, hour, minute)

  d0 <- dat.stn$date[1]
  d1 <- tail(dat.stn$date, 1)
  ndays <- d1 - d0
  if (ndays < 21) {
    the.station <- dat.stn$station[1]
    msg <- glue::glue(
      "Records for station {the.station} span {ndays} days but \\
       at least 21 days are required to calculate FFDI")
    warning(msg, immediate. = TRUE)
    return(NULL)
  }

  # Ensure only one record per time point (this will arbitrarily
  # take the first from any duplicate set)
  dat.stn <- dat.stn %>%
    dplyr::distinct(date, hour, minute, .keep_all = TRUE)

  # Treat missing rainfall values as zero (least worst option)
  dat.stn$precipitation <- .na2zero(dat.stn$precipitation)

  # Add dummy records for any missing days. Dates of dummy records
  # are returned as an attribute.
  dat.stn <- .add_missing_days(dat.stn)
  dates.added <- attr(dat.stn, "dates.added", exact = TRUE)
  attr(dat.stn, "dates.added") <- NULL

  # Tmax by calendar day
  #
  dat.daily <- dat.stn %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(tmaxdaily = .max_with_na(temperature)) %>%
    dplyr::ungroup()

  # Add daily rainfall data
  r <- calculate_daily_rainfall(dat.stn, datatype = datatype)
  dat.daily <- dplyr::left_join(dat.daily, r, by = c("date" = "raindate"))

  # Guard against a missing daily rainfall value which can arise
  # when the last rain day's data did not include the full
  # 09:01-09:00 period
  dat.daily$precipdaily <- .na2zero(dat.daily$precipdaily)

  # Ensure record order
  dat.daily <- dat.daily %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date)


  dat.daily <- dat.daily %>%
    calculate_kbdi(average.rainfall = av.rainfall, assume.order = TRUE) %>%
    calculate_drought_factor(assume.order = TRUE)


  # Join daily values to original time-step data
  dat.stn <- dat.stn %>%
    dplyr::arrange(date, hour, minute) %>%
    dplyr::left_join(dat.daily, by = "date") %>%

    # Add calculated FFDI values.
    # Missing values in any of the input variables will propagate
    # through to FFDI.
    dplyr::mutate(ffdi = round(
      2 * exp(0.987 * log(drought) - 0.45 +
                0.0338 * temperature +
                0.0234 * windspeed -
                0.0345 * relhumidity),
      digits = 1))

  # Remove any dummy records that were added for missing days
  if (length(dates.added) > 0) {
    ii <- dat.stn$date %in% dates.added
    dat.stn <- dat.stn[!ii, ]
  }

  # Return result
  dat.stn
}


#' Calculate daily KBDI values from data for a single weather station
#'
#' Calculates daily values for the Keetch-Byram drought index. Following Finkele
#' et al. (2006), KBDI is calculated as yesterday's KBDI plus
#' evapo-transpiration minus effective rainfall. Input is a data frame of daily
#' weather data. This must have columns: date, precipdaily,
#' tmaxdaily. Optionally, it can also have a column: kbdi. If present,
#' calculations will be done for the most recent block of records with missing
#' values, with preceding records being used to initialize the KBDI time series.
#' If a kbdi column is absent, or present but with all missing values,
#' calculations will be performed for all records using the default
#' initialization in which KBDI is set to its maximum value (203.2).
#'
#' Dealing with missing values: Nny missing daily precipitation values are set
#' to zero and a message is issued, Missing daily temperature values are left
#' as-is. The value of KBDI depends on the previous day's value. A missing value
#' in the daily time series for temperature leads to a missing value for KBDI,
#' which would then propagate to the next and all subsequent days. To avoid
#' this, if the KBDI value for the previous day is missing, the function uses
#' the most recent non-missing value from the previous 30 days. If no
#' non-missing value is available, the maximum value of 203.2 is used.
#'
#' @param dat.daily A data frame of daily weather data. This should be for a
#'   single weather station. If a 'station' column is present this will be
#'   checked, otherwise it is assumed. Must have columns: date,
#'   precipdaily, tmaxdaily. Optionally, it can also have column: kbdi.
#'   Additional columns are permitted but will be ignored. See Description for
#'   details.
#'
#' @param average.rainfall Reference value for average annual rainfall.
#'
#' @param assume.order If \code{TRUE} the daily data are assumed to be in
#'   calendar order and form an uninterrupted time series. If \code{FALSE}
#'   (default) this will be checked. This option provides for more efficient
#'   processing as part of a pipeline, but should be left at the default when
#'   the function is called directly.
#'
#' @return A data frame with columns: date, kbdi.
#'
#' @export
#'
calculate_kbdi <- function(dat.daily,
                           average.rainfall,
                           assume.order = FALSE) {

  colnames(dat.daily) <- tolower(colnames(dat.daily))

  # If there is a station column, check there is only a single station
  if (("station" %in% colnames(dat.daily)) &&
      dplyr::n_distinct(dat.daily$station) > 1) {
    stop("This function presently only works with data for a single station")
  }

  RequiredCols <- c("date", "precipdaily", "tmaxdaily")
  if (!all(RequiredCols %in% colnames(dat.daily))) {
    stop("Input data frame should have columns: date, precipdaily, tmaxdaily")
  }

  if (!assume.order) {
    check <- .check_datetimes(dat.daily, is.daily = TRUE)[[1]]
    if (!check$ok) stop(check$err)
  }

  if (anyNA(dat.daily$precipdaily)) {
    message("calculate_kbdi: setting missing precipitation values to zero")
    dat.daily$precipdaily <- .na2zero(dat.daily$precipdaily)
  }

  InputKBDI <- "kbdi" %in% colnames(dat.daily)

  if (InputKBDI) {
    # Identify most recent period with no KBDI values
    start.rec <- .find_na_tail(dat.daily$kbdi)
    if (is.na(start.rec)) {
      # Last rec has KBDI value - nothing to do
      message("Final day has KBDI value already. Nothing to do.")
      return(dat.daily)
    }
  } else {
    start.rec <- 1
  }

  Ndays <- nrow(dat.daily)
  ET <- numeric(Ndays)

  if (InputKBDI) {
    Kday <- dat.daily$kbdi
  } else {
    Kday <- numeric(Ndays)
  }

  precipdaily.eff <- calculate_effective_rainfall(dat.daily$precipdaily)

  # Term 3 in ET equation, annual rainfall influence
  ET3 <- 1 + 10.88 * exp(-.001736 * average.rainfall)

  # Term 4 in ET equation
  ET4 <- 1e-3

  # First step requires at least two days of KBDI values.
  if (start.rec == 1) {
    # Starting from scratch - set the first two days to maximum
    # dryness value
    Kday[1:2] <- 203.2
  } else if (start.rec == 2) {
    #
    Kday[2] <- Kday[1]
  }

  # start.rec should be >= 3 for loop below
  start.rec <- max(3, start.rec)

  # Helper for KBDI loop below
  clampK <- function(x) {
    ifelse(is.na(x), NA, min(203.2, max(0, round(x, digits = 1))))
  }

  for (i in start.rec:Ndays) {
    Kprev <- Kday[i-1]

    if (is.na(Kprev)) {
      # [Hamish - comment from MATLAB code]
      # Missing Tmax or precip.eff values result in all subsequent
      # ET and KBDI values being set to missing because of the lagged effect
      # of these variables. We need to specify when it is OK to ignore previous
      # missing values.
      #
      # In the approach used here, if Tmax and rain values are present
      # for days i and i-1, then we use the most recent non-missing
      # KBDI value from not more than 30 days previously. If no previous
      # data are available for that period, we set previous KBDI to
      # maximum deficit of 203.2
      #
      Krecent <- na.omit( Kday[(i-2):max(1, i-30)] )
      if (length(Krecent) > 0) {
        # most recent non-missing value
        Kprev <- Krecent[1]
      } else {
        # no values available - set to max deficit
        Kprev <- 203.2
      }
    }

    ET1 <- 203.2 - Kprev
    ET2 <- 0.968 * exp(0.0875 * dat.daily$tmaxdaily[i-1] + 1.5552) - 8.30
    ET[i] <- ((ET1 * ET2) / ET3) * ET4

    Kday[i] <- clampK( Kprev - precipdaily.eff[i] + ET[i] )
  }

  dat.daily$kbdi <- Kday

  dat.daily
}


#' Calculate daily drought factor values from data for a single weather station
#'
#' Calculates daily values for drought factor.Input is a data frame of daily
#' weather and KBDI data. This must have columns: date, precipdaily,
#' tmaxdaily, kbdi. Optionally, it can also have a column: drought. If present,
#' calculations will be done for the most recent block of records with missing
#' values, with preceding records being used to initialize the drought factor
#' time series. If a drought column is absent, or present but with all missing
#' values, calculations will be performed for all records using the default
#' initialization in which KBDI is set to its maximum value (203.2).
#'
#' Drought factor is based on a moving window of the previous 20 days. Within
#' this window we search for a rainfall event, defined as consecutive days, each
#' with rain >2mm. If an event occurs we determine the total rainfall over the
#' event, and the number of days since the day of highest rainfall within the
#' event. The resulting values are then combined with KBDI in some darkly
#' mysterious way.
#'
#' @param dat.daily A data frame of daily weather data. This should be for a
#'   single weather station. If a 'station' column is present this will be
#'   checked, otherwise it is assumed. Must have columns: date,
#'   precipdaily, kbdi. Optionally, it can also have column: drought. Additional
#'   columns are permitted but will be ignored. See Description for details.
#'
#' @param assume.order If \code{TRUE} the daily data are assumed to be in
#'   calendar order and form an uninterrupted time series. If \code{FALSE}
#'   (default) this will be checked. This option provides for more efficient
#'   processing as part of a pipeline, but should be left at the default when
#'   the function is called directly.
#'
#' @return A data frame with columns: date, kbdi, drought.
#'
#' @export
#'
calculate_drought_factor <- function(dat.daily, assume.order = FALSE) {

  colnames(dat.daily) <- tolower(colnames(dat.daily))

  # If there is a station column, check there is only a single station
  if (("station" %in% colnames(dat.daily)) &&
      dplyr::n_distinct(dat.daily$station) > 1) {
    stop("This function presently only works with data for a single station")
  }

  RequiredCols <- c("date", "precipdaily", "kbdi")
  if (!all(RequiredCols %in% colnames(dat.daily))) {
    stop("Input data frame must have columns: ",
         paste(RequiredCols, collapse = ", "))
  }

  if (!assume.order) {
    check <- .check_datetimes(dat.daily, is.daily = TRUE)[[1]]
    if (!check$ok) stop(check$err)
  }

  Ndays <- nrow(dat.daily)

  InputDF <- "drought" %in% colnames(dat.daily)

  # Check there are enough days to do something
  if (Ndays < 21) {
    message("Too few days (min is 21) to calculate drought factor")

    # Add a drought column if there is not already one and return
    if (!InputDF) dat.daily$drought <- NA_real_
    return(dat.daily)
  }

  if (InputDF) {
    DFday <- dat.daily$drought

    # Identify most recent period with no drought values
    start.rec <- .find_na_tail(dat.daily$drought)
    if (is.na(start.rec)) {
      # Last rec has drought value - nothing to do
      message("Final day has drought factor value already. Nothing to do.")
      return(dat.daily)
    }
  } else {
    DFday <- rep(NA_real_, Ndays)
    start.rec <- 21
  }

  # start.rec should be >= 21 for loop below
  start.rec <- max(21, start.rec)

  # Only rainfall of 2mm or more is considered
  prDF <- ifelse(dat.daily$precipdaily > 2, dat.daily$precipdaily, 0)

  for (i in start.rec:Ndays) {
    if (is.na(dat.daily$kbdi[i]) || is.na(dat.daily$precipdaily[i])) {
      DFday[i] <- NA

    } else {
      # Find most recent rainfall event not more than 20 days ago
      ii <- (i-1):(i-20)

      r20 <- prDF[ii]
      r20.flag <- prDF[ii] > 0

      # Look for instances of two consecutive days with rain
      is.event <- r20.flag & dplyr::lead(r20.flag)

      # last is.event element will be NA, change to FALSE
      is.event[length(r20.flag)] <- FALSE

      if (!any(is.event)) {
        # No runs of two or more rainy days
        xraw <- 1

      } else {
        # start index of most recent event
        ev.start <- which(is.event)[1]

        # end index of most recent event
        ii <- which(!r20.flag)
        ii <- ii[ii > ev.start]
        ev.end <- ifelse(length(ii) == 0, length(r20.flag), ii[1] - 1)

        # event rainfall
        E <- r20[ev.start:ev.end]
        P <- sum(E)

        # index of maximum rainfall in the window
        # (days since most rain)
        Emax.i <- which.max(E) + ev.start - 1

        # Following Finkele et al. (2006)
        Emax.term <- ifelse(Emax.i == 1, 0.8, Emax.i - 1)
        xraw <- (Emax.term^1.3) / (Emax.term^1.3 + P - 2)
      }

      if (dat.daily$kbdi[i] >= 20) {
        xlim <- 75 / (270.525 - 1.267 * dat.daily$kbdi[i])
      } else {
        xlim <- 1 / (1 + 0.1135 * dat.daily$kbdi[i])
      }

      x <- min(xraw, xlim)

      xfactor <- (41 * x^2 + x) / (40 * x^2 + x + 1)

      DFbase <- 10.5* (1.0 - exp(-(dat.daily$kbdi[i] + 30) / 40))
      DFraw <- DFbase * xfactor

      # If DFraw is NA, allow this to propagate to DFday
      DFday[i] <- round( min(10, DFraw), digits = 1)
    }
  }

  dat.daily$drought <- DFday

  dat.daily
}


#' Calculate effective rainfall values from daily values
#'
#' For KBDI calculation (Finkele et al. 2006), the first 5mm of rain
#' do not count towards evapo-transpiration. If it rains less than 5mm over
#' several days, a running balance is kept. Once this balance exceeds 5mm, the
#' first 5mm is removed for canopy interception/runoff and the remainder
#' constitutes effective rainfall. Any day without rain resets the balance to
#' zero.
#'
#' @param precipdaily Vector of daily precipitation values.
#'
#' @param start.balance Starting balance. Should be a value between 0 and 5
#'   (default is 0). A value outside this range will be silently set to 0 or 5.
#'
#' @return A vector of effective rainfall values.
#'
#' @export
#'
calculate_effective_rainfall <- function(precipdaily, start.balance = 0) {
  if (anyNA(precipdaily)) stop("Missing values in input vector for daily precipitation")

  start.balance <- min(5, max(0, start.balance[1]))

  Ndays <- length(precipdaily)
  peff <- numeric(Ndays)
  bal <- start.balance

  for (i in 1:Ndays) {
    bal.prev <- bal

    if (precipdaily[i] == 0) {
      # reset for a day with no rain
      bal <- 0
    } else {
      if (bal.prev == 0) {
        # some rain this day and
        # none previously
        if (precipdaily[i] > 5) {
          # remove interception/runoff then set as Peff
          peff[i] <- precipdaily[i] - 5
          # Set bal to 5 so any rain in next day is added
          # without removing 5
          bal <- 5
        } else {
          # no effective rain but the balance increases
          bal <- precipdaily[i]
        }
      } else if (bal.prev < 5) {
        # some rain this day and
        # less than five previously
        if (precipdaily[i] + bal.prev > 5) {
          # remove interception/runoff, then set as Peff
          peff[i] <- precipdaily[i] + bal.prev - 5
          # Set bal to 5 so any rain in next day is added
          # without removing 5
          bal <- 5
        } else {
          # still haven't exceeded 5mm
          bal <- precipdaily[i] + bal.prev
        }
      } else {
        # interception/runoff has already been removed
        # so add all rain to Peff keep balance at 5
        peff[i] <- precipdaily[i]
        bal <- 5
      }
    }
  }

  peff
}


#' Aggregate rainfall values to daily time steps
#'
#' This is a helper function called by other functions when
#' calculating fire-related variables. It aggregates sub-daily rainfall data to
#' daily values, taking into account the different conventions used for AWS and
#' Syoptic data sources. Each value for each day is the total rainfall recorded
#' from 09:01 that day to 09:00 the following day. If there are any missing days
#' in the time series, the function will either stop with an error or return
#' \code{NULL} depending on the value of the \code{on.error} argument.
#'
#' @param dat A data set of sub-daily weather records for either an AWS or a
#'   Synoptic source.
#'
#' @param datatype Source type of data. Default is to guess based on data
#'   values.
#'
#' @param crop If \code{TRUE} (default), discard the rainfall for an initial
#'   part day.
#'
#' @param on.error Action to perform if there is any interruption in the time
#'   series. If \code{'stop'}, the function will stop with an error message. If
#'   \code{'null'}, the function will return NULL.
#'
#' @return A data frame with columns: station (if present in the input data),
#'   date, precipdaily.
#'
#' @export
#'
calculate_daily_rainfall <- function(dat,
                                     datatype = c("guess", "aws", "synoptic"),
                                     crop = FALSE,
                                     on.error = c("stop", "null")) {

  on.error <- match.arg(on.error)

  colnames(dat) <- tolower(colnames(dat))

  RequiredCols <- c("date", "hour", "minute", "precipitation")
  ii <- RequiredCols %in% colnames(dat)
  if (!all(ii)) {
    stop("Missing required column(s): ",
         paste(RequiredCols[!ii], collapse = ", "))
  }

  HasStation <- "station" %in% colnames(dat)
  if (!HasStation) dat$station <- 0

  datatype <- match.arg(tolower(datatype), choices = c("guess", "aws", "synoptic"))
  if (datatype == "guess") {
    # check first station
    the.stn <- dat$station[1]
    datatype <- .guess_data_type(dplyr::filter(dat, station == the.stn))
  }

  # Helper function to aggregate rain-day values
  fn_rainday <- function(rain) {
    rain <- na.omit(rain)
    if (length(rain) == 0) {
      0
    } else {
      ifelse(datatype == "aws", max(rain), sum(rain))
    }
  }

  stns <- unique(dat$station)

  res <- lapply(stns, function(the.stn) {
    dat.stn <- dplyr::filter(dat, station == the.stn)

    check <- .check_datetimes(dat.stn, is.daily = FALSE)[[1]]

    if (!check$ok) {
      if (on.error == "null") {
        return(NULL)
      } else {
        gap.msg <- ""
        if (length(check$gaps) > 0) gap.msg <- paste(check$gaps, collapse = " ")
        msg <- glue::glue("Problem with data for station {the.stn}
                        {check$err}
                        {gap.msg}")
        stop(msg)
      }
    }

    dat.stn <- dat.stn %>%
      dplyr::arrange(date, hour, minute) %>%

      dplyr::mutate(raindate = date,
                    timestr = sprintf("%02d%02d", hour, minute))

    # Do this bit outside of dplyr to avoid it spuriously converting
    # dates to integers
    ii <- dat.stn$timestr < "0901"
    dat.stn$raindate[ii] <- dat.stn$raindate[ii] - 1

    min.raindate <- min(dat.stn$raindate)

    dat.stn <- dat.stn %>%
      dplyr::group_by(raindate) %>%
      dplyr::summarize(precipdaily = fn_rainday(precipitation))


    if (crop) dat.stn <- dplyr::filter(dat.stn, raindate > min.raindate)

    dat.stn
  })

  # Check for problems
  if (on.error == "null" && any(sapply(res, is.null))) {
    return(NULL)
  }

  # Combine results for stations
  res <- dplyr::bind_rows(res)

  # If there was no station column in the input, remove the temp column
  if (!HasStation) res$station <- NULL

  res
}


# Guess data type (aws or synoptic) for a set of weather records based
# on the presence or absence of sub-hourly time steps.
#
.guess_data_type <- function(dat) {

  x <- dat %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarize(ntimesteps = dplyr::n_distinct(minute))

  if (any(x$ntimesteps > 1)) {
    datatype <- "aws"
  } else {
    datatype <- "synoptic"
  }

  datatype
}


# Check that a data set represents an uninterrupted time series
#
.check_datetimes <- function(dat, is.daily) {
  if (missing(is.daily)) stop("Argument 'is.daily' (logical) must be provided")

  colnames(dat) <- tolower(colnames(dat))
  if (!("date" %in% colnames(dat))) {
    stop("Column 'date' is required")
  }

  HasStation <- ("station" %in% colnames(dat))

  if (HasStation) {
    if (anyNA(dat$station)) stop("station column should not contain missing values")
  } else {
    dat$station <- -1
  }

  # ungroup just in case
  dat <- dplyr::ungroup(dat)

  # Vars to use for ordering records
  ovars <- "date"
  if ("hour" %in% colnames(dat)) ovars <- c(ovars, "hour")
  if ("minute" %in% colnames(dat)) ovars <- c(ovars, "minute")

  # Run check for each station
  checks <- lapply(unique(dat$station), function(stn) {
    dat.stn <- dat %>% dplyr::filter(station == stn)

    # Default check value
    res <- list(station = stn,
                ok = TRUE,
                err = NULL,
                gaps = NULL)

    # If less than two records, gaps and order do not apply
    if (nrow(dat.stn) < 2) {
      return(res)
    }

    # If only daily records are expected, check and return
    # early if that is not the case
    if (is.daily && dplyr::n_distinct(dat.stn$date) < nrow(dat.stn)) {
      res$ok <- FALSE
      res$err <- "Expected only one record per day"
      return(res)
    }

    dat.stn <- dplyr::arrange_at(dat.stn, ovars)

    # Check that there are no missing days
    dat.stn <- dat.stn %>%
      dplyr::mutate(diff = as.integer(date - dplyr::lag(date)))

    # First record is ignored because there is no prior date
    okdiffs <- c(TRUE, dat.stn$diff[-1] %in% 0:1)
    if (any(!okdiffs)) {
      res$ok <- FALSE
      res$err <- "Gap(s) in time series"
      res$gaps <- dat.stn$date[!okdiffs]
    }

    res
  })

  checks
}


# Add records for any missing dates to a data frame of weather data
# and record the dates as an attribute.
# Note: the records should be for a single weather station.
#
.add_missing_days <- function(dat) {
  colnames(dat) <- tolower(colnames(dat))
  if ( !("date" %in% colnames(dat)) ) {
    stop("Data frame must have a column 'date'")
  }

  # If a station column is present there must be only one value
  station <- NA
  station.col <- match("station", tolower(colnames(dat)))
  if (!is.na(station.col)) {
    station <- unique(dat[, station.col])
    if (length(station) != 1) {
      stop("Data records should be for just one station")
    }
  }

  dates <- unique(dat$date)

  # Identify any missing between earliest and latest date
  dates <- setdiff(seq(min(dates), max(dates), by = "1 day"),
                   dates)

  if (length(dates) > 0) {
    # setdiff gives a vector of days-since-epoch values instead
    # of Date objects, so fix that:
    dates <- as.Date(dates, origin = "1970-01-01")

    extras <- data.frame(date = dates)
    extras$hour <- 0
    extras$minute <- 0

    if (!is.na(station)) extras$station <- station

    dat <- dplyr::bind_rows(dat, extras)
    dat <- dplyr::arrange(dat, date, hour, minute)

    attr(dat, "dates.added") <- dates

  } else {
    # No dates added. Set attribute to empty Date vector
    attr(dat, "dates.added") <- as.Date(x = integer(0), origin = "1970-01-01")
  }

  dat
}


# Version of max that returns NA instead of -Inf if x is
# empty or all values are NA
.max_with_na <- function(x) {
  stopifnot(is.numeric(x))

  # If all values are NA, return NA instead of -Inf
  x <- na.omit(x)
  if (length(x) == 0) NA_real_
  else max(x, na.rm = TRUE)
}


# Convert NA values in a vector to zero
.na2zero <- function(x) {
  ifelse(is.na(x), 0, x)
}


# Given a vector, check for a block of one or more missing values at the tail
# and, if found, return the index for the first missing value in that block.
# Return NA if no such block is present.
.find_na_tail <- function(x) {
  N <- length(x)
  if (!is.na(x[N])) {
    NA
  } else {
    i <- match(FALSE, rev(is.na(x)), nomatch = N+1)
    N - i + 2
  }
}

