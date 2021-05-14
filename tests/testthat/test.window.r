test_that("window works with empy Dataset", {
  expect_error(window(Dataset(), start=c(1990,1)), NA)
})

test_that("window works with a single ts Dataset", {
  x <- Dataset()
  x[["A"]] <- stats::ts(rep(0, 100), start=c(1990, 1), frequency = 4)

  y <- expect_error(window(x, start=c(2000, 1)), NA)
  expect_s4_class(y, "Dataset")
  expect_true("A" %in% names(y))
  expect_equal(stats::start(y[["A"]]), c(2000, 1))
})


test_that("window works with a multiple ts Dataset", {
  x <- Dataset()
  x[["A"]] <- stats::ts(rep(0, 100), start=c(1990, 1), frequency = 4)
  x[["B"]] <- stats::ts(rep(0, 100), start=c(1999, 1), frequency = 12)

  y <- expect_error(window(x, start=c(2000, 1)), NA)
  expect_s4_class(y, "Dataset")
  expect_true("A" %in% names(y))
  expect_true("B" %in% names(y))
  expect_equal(stats::start(y[["A"]]), c(2000, 1))
  expect_equal(stats::start(y[["B"]]), c(2000, 1))
})

test_that("window works with a multiple ts Dataset", {
  x <- Dataset()
  x[["A"]] <- stats::ts(rep(0, 4), start=c(1990, 1), frequency = 4)
  x[["B"]] <- stats::ts(rep(0, 8), start=c(1990, 1), frequency = 4)

  y <- expect_warning(window(x, end=c(1991, 1)))
  expect_s4_class(y, "Dataset")
  expect_true("A" %in% names(y))
  expect_true("B" %in% names(y))
})


