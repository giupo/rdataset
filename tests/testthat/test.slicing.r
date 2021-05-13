test_that("slicing works as expected", {
  d <- suppressWarnings(Dataset())
  tt <- ts(rep(0, 10), start = c(1990, 1), freq=4)
  d["TEST"] <- tt
  expect_true("TEST" %in% names(d))
  d[["TEST2"]] <- tt
  expect_true("TEST2" %in% names(d))
})

test_that("slicing with multiple names", {
  data <- list()
  data[["A"]] <- ts(c(1, 2, 3), start = c(1990, 1), freq=4)
  data[["B"]] <- ts(c(1, 2, 3), start = c(1990, 1), freq=4)
  d <- as.dataset(data)
  d[[names(data)]] <- data
  expect_true(all(c("A", "B") %in% names(d)))
})

test_that("boolean operators work on dataset", {
  ds <- Dataset()
  ds["A"] <- ts(c(1, 2, 3), start = c(1990, 1), freq=4)
  ds["B"] <- ts(c(0,0,0), start = c(1990, 1), freq=4)
  ds["C"] <- ds[["B"]] - ds[["A"]]
  out <- ds[ds == 0]
  expect_true("B" %in% names(out))
  out <- ds[ds > 0]
  expect_true("A" %in% names(out))
  out <- ds[ds < 0.1]
  expect_true("C" %in% names(out))
})

test_that("I can use or omit which when subsetting", {
  ds <- Dataset()
  ds["A"] <- ts(c(1, 2, 3), start = c(1990, 1), freq=4)
  ds["B"] <- ts(c(0,0,0), start = c(1990, 1), freq=4)
  ds["C"] <- ds[["B"]] - ds[["A"]]
  out1 <- ds[ds == 0]
  out2 <- ds[which(ds == 0)]
  expect_true("B" %in% names(out1))
  expect_true("B" %in% names(out2))
})


test_that("failig to match subsetting returns an empty Dataset", {
  d <- as.dataset(list(A=1))
  idx <- rep(FALSE, length(d))
  expect_equal(length(d[idx]), 0)
})

test_that("subsetting with a zero numeric array returns an empty Dataset", {
  d <- Dataset()
  idx <- numeric(0)
  expect_equal(length(d[idx]), 0)
})

test_that("subsetting with inconsistent numeric array returns an error", {
  d <- Dataset()
  idx <- c(8,9, 10)
  expect_error(d[idx])
})

test_that("Setting to null an object removes it from Dataset", {
  d <- as.dataset(list(TS1=1))
  origLength <- length(d)
  d["TS1"] <- NULL
  expect_equal(length(d), origLength - 1)
})


test_that("can't subset with list()", {
  d <- Dataset()
  expect_error(d[list(a=1)], "can't subset")
})


test_that("I can access series with $", {
  ds <- Dataset()
  ds["A"] <- A <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["B"] <- B <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  expect_equal(ds$A, A)
  expect_equal(ds$B, B)
  expect_equal(ds$`A B`, ds[[c("A", "B")]])
})

