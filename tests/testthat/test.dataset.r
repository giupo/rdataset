
test_that("as.dataset works with list, characters", {
  l <- list(
    A=ts(c(1, 2, 3), start = c(1990, 1), freq=4),
    B=ts(c(1, 2, 3), start = c(1990, 1), freq=4))

  ds <- as.dataset(l)
  expect_true(is.dataset(ds))
  expect_true(all(c("A", "B") %in% names(ds)))
})


test_that("I can use abs function over a Dataset", {
  ds <- Dataset()
  ds["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  ds["B"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 4)
  ads <- abs_ds(ds)
  expect_true(all(ads[["A"]] > 0))
})

test_that("as.list on datasets works as expected", {
  ds <- Dataset()
  ds["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), freq = 4)
  ds["B"] <- ts(c(0, 0, 0), start = c(1990, 1), freq = 4)
  l <- as.list(ds)
  expect_true(is.list(l))
  expect_true(all(unlist(lapply(l, stats::is.ts))))
  expect_true(!all(unlist(lapply(l, is.dataset))))
  expect_true("A" %in% names(l))
  expect_true("B" %in% names(l))
  expect_true(all(names(l) %in% names(ds)))
  expect_equal(ds[["A"]], l$A)
  expect_equal(ds[["B"]], l$B)
})

test_that("Union of datasets works as expected", {
  ds1 <- Dataset()
  ds1["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  ds1["B"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 4)
  ds2 <- Dataset()
  ds2["C"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  ds2["D"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 4)

  ds <- union(ds1, ds2)
  
  expect_true(all(names(ds1) %in% names(ds)))
  expect_true(all(names(ds2) %in% names(ds)))

  expect_true(all(unlist(lapply(as.list(ds), stats::is.ts))))
  expect_true(!all(unlist(lapply(as.list(ds), is.dataset))))
})


test_that("looking for wrong names yields a warning and an empty Dataset", {
  d <- Dataset()
  idx <- c("A", "B", "C")
  expect_warning(
    expect_warning(
      expect_warning(
        expect_equal(length(d[idx]), 0))))
})


test_that("I can produce an xlsx from a Dataset (bycol)", {
  skip_if(.Platform$OS.type == "windows")
  skip_if_not_installed("writexl")
  ds <- Dataset()

  ds["TS1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["TS4"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["TS12"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)
  ds["TS1-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["TS4-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["TS12-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)

  tmpfile <- tempfile(fileext=".xlsx")
  on.exit(unlink(tmpfile, force = TRUE))

  to_xlsx(ds, tmpfile, bycol = TRUE)
  expect_true(file.info(tmpfile)$size > 0)
})

test_that("I can produce an xlsx from a Dataset", {
  skip_if(.Platform$OS.type == "windows")
  skip_if_not_installed("writexl")
  ds <- Dataset()

  ds["TS1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["TS4"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["TS12"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)
  ds["TS1-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["TS4-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["TS12-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)

  tmpfile <- tempfile(fileext=".xlsx")
  on.exit(unlink(tmpfile, force = TRUE))

  to_xlsx(ds, tmpfile, bycol = FALSE)
  expect_true(file.info(tmpfile)$size > 0)
})

test_that("I can produce a csv from a Dataset", {
  skip_if(.Platform$OS.type == "windows")
  skip_if_not_installed("writexl")
  ds <- Dataset()

  ds["TS1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["TS4"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["TS12"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)
  ds["TS1-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  ds["TS4-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["TS12-1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)

  x <- expect_error(to_csv(ds), NA)
  expect_type(x, "character")
})



test_that("Init with a non existing directory raises a warning and an error", {
  expect_error(expect_warning(Dataset("/io/non/esisto")))
})


test_that("`as.vector` returns a list representtion of this Dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3))
  d["B"] <- ts(c(1, 2, 3))
  expect_type(as.vector(d), "list")
  expect_equal(as.vector(d), list(A=d[["A"]], B=d[["B"]]))
})


test_that("differences from a dataset with different names raises a warning", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3))
  d1["B"] <- ts(c(1, 2, 3))
  d2 <- Dataset()
  d2["A"] <- ts(c(1, 2, 3))
  d2["B"] <- ts(c(1, 2, 3))
  d2["C"] <- ts(c(1, 2, 3))

  expect_warning(d1 - d2)
  expect_warning(d2 - d1)
})


test_that("differences from a dataset with zero common names raises an error", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3))
  d1["B"] <- ts(c(1, 2, 3))
  d2 <- Dataset()
  d2["C"] <- ts(c(1, 2, 3))
  d2["D"] <- ts(c(1, 2, 3))

  expect_error(d1-d2)
})

test_that("Expect output from show with > 1 objects", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3))
  d["B"] <- ts(c(1, 2, 3))

  expect_output(show(d), "Dataset with 2 objects")
})

test_that("Expect output from show with 1 object", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3))

  expect_output(show(d), "Dataset with 1 object")
  
})

test_that("saveDataset behaves like expected", {
  output <- tempfile()
  on.exit(file.remove(output))

  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3))
  d["B"] <- ts(c(1, 2, 3))

  expect_error(saveDataset(d, output), NA)
})


test_that("fullsummary e shortsummary provide an output", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  d["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d["C"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)
  # to test freq bins...
  d["A1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)
  d["B1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d["C1"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)

  expect_output(shortSummary(d))
  expect_output(fullSummary(d))
})

test_that("as.dataset on a dataset returns a dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3))
  d["B"] <- ts(c(1, 2, 3))

  x <- as.dataset(d)
  expect_true(is.dataset(x))
  expect_equal(names(d), names(x))
  for(name in names(d)) {
    expect_equal(d[[name]], x[[name]])
  }
})
