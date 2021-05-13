test_that("I can diff two datasets", {
  ds1 <- Dataset()
  ds2 <- Dataset()

  ds1["A"] <- ts(c(1, 2, 3), start = c(1990, 1), freq=4)
  ds2["A"] <- ts(c(1, 2, 3), start = c(1990, 1), freq=4)

                                        # warning due lack of multicore
  diff <- ds1 - ds2
  expect_true("A" %in% names(diff))
  expect_equal(sum(abs(diff[["A"]])),0)
})



test_that("comparators return a logical values", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3))
  d1["B"] <- ts(c(1, 2, 3))

  x <- d1 <= 2
  expected <- c(A=TRUE, B=TRUE)
  expect_true(is.logical(x))
  expect_equal(x, expected)

  x <- d1 >= 2
  expected <- c(A=TRUE, B=TRUE)
  expect_true(is.logical(x))
  expect_equal(x, expected)


  x <- d1 <= -2
  expected <- c(A=FALSE, B=FALSE)
  expect_true(is.logical(x))
  expect_equal(x, expected)

  x <- d1 >= 4
  expected <- c(A=FALSE, B=FALSE)
  expect_true(is.logical(x))
  expect_equal(x, expected)
})
