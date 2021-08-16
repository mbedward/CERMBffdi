test_that(".max_with_na", {
  fn <- CERMBffdi:::.max_with_na

  x <- numeric(0)
  expect_true(is.na(fn(x)))

  x <- rep(NA_real_, 10)
  expect_true(is.na(fn(x)))

  x[10] <- 42
  expect_equal(fn(x), 42)
})


test_that("convert NA to zero in vector", {
  Ndata <- 100
  Nmiss <- 20

  ii <- sample(Ndata, Nmiss)
  x <- 1:Ndata
  x[ii] <- NA

  x2 <- CERMBffdi:::.na2zero(x)

  expect_false(anyNA(x2))
  expect_equal(length(x2), Ndata)
  expect_equal(sum(x2 == 0), Nmiss)
})


test_that("find tail missing values in vector", {
  fn <- CERMBffdi:::.find_na_tail

  x <- 1:20
  expect_true(is.na(fn(x)))

  x[1] <- NA
  expect_true(is.na(fn(x)))

  x[20] <- NA
  expect_equal(fn(x), 20)

  x[11:18] <- NA
  expect_equal(fn(x), 20)

  x[11:20] <- NA
  expect_equal(fn(x), 11)
})

