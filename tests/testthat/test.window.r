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

test_that("window raises a warning ts Dataset", {
  x <- Dataset()
  x[["A"]] <- stats::ts(rep(0, 4), start=c(1990, 1), frequency = 4)
  x[["B"]] <- stats::ts(rep(0, 8), start=c(1990, 1), frequency = 4)

  y <- expect_warning(window(x, end=c(1991, 1)))
  expect_s4_class(y, "Dataset")
  expect_true("A" %in% names(y))
  expect_true("B" %in% names(y))
})

test_that("window leaves the series alone if stats::window mess up", {
  skip_if_not_installed("mockery")

  stat_window_mock <- mockery::mock(stop("error"))
  mockery::stub(window.Dataset, "stats::window", stat_window_mock)

  x <- Dataset()
  x[["A"]] <- stats::ts(rep(0, 4), start=c(1990, 1), frequency = 4)
  y <- expect_error(window(x, start=c(1990, 2)), NA)
  expect_identical(x[["A"]], y[["A"]])
})

