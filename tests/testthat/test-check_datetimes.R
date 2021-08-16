.make_test_data <- function(years, months, days, hours = NULL, stations = 1234) {
  x <- data.frame(station = stations,
                  date = as.Date(sprintf("%d-%d-%d", years, months, days)))

  if (!is.null(hours)) {
    x$hours <- hours
  }

  x
}

test_that("An uninterrupted time series checks ok", {
  dat <- .make_test_data(station = 1234,
                         years = 2020,
                         months = c(rep(4, 30), rep(5, 31), rep(6, 30)),
                         days = c(1:30, 1:31, 1:30))

  check <- CERMBffdi:::.check_datetimes(dat, is.daily = TRUE)[[1]]

  expect_true(check$ok)
  expect_null(check$err)
  expect_null(check$gaps)
})


test_that("Multi-station data produces the correct list structure", {
  stns <- 1001:1004

  dat <- .make_test_data(station = rep(stns, each = 10),
                         years = 2020,
                         months = 4,
                         days = rep(1:10, length(stns)))

  check <- CERMBffdi:::.check_datetimes(dat, is.daily = TRUE)

  expect_length(check, length(stns))

  for (i in 1:4) expect_equal(check[[i]]$station, stns[i])
})


test_that("Presence of sub-daily data is checked properly", {
  dat <- .make_test_data(stations = 1234,
                         years = 2020,
                         months = 4,
                         days = rep(1:10, each = 3),
                         hours = rep(c(9, 12, 15), 10))

  check <- CERMBffdi:::.check_datetimes(dat, is.daily = TRUE)[[1]]
  expect_false(check$ok)

  check <- CERMBffdi:::.check_datetimes(dat, is.daily = FALSE)[[1]]
  expect_true(check$ok)
})


test_that("Gaps in a time series are identified", {
  dat <- .make_test_data(station = 1234,
                         year = 2020,
                         month = rep(1:3, each = 10),
                         day = rep(11:20, 3))

  check <- CERMBffdi:::.check_datetimes(dat, is.daily = TRUE)[[1]]

  expect_false(check$ok)
  expect_match(check$err, "[Gg]ap")
  expect_equal(check$gaps, c(as.Date("2020-02-11"), as.Date("2020-03-11")))
})


