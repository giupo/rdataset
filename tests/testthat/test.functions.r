
test_that("union of datasets behaves like a regular union", {
  ds1 <- Dataset()
  ds1["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  ds1["B"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 4)

  ds2 <- Dataset()
  ds2["B"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  ds2["D"] <- ts(c(0,0,0), start = c(1990, 1), frequency = 4)

  ds <- union(ds1, ds2)

  expect_equal(length(ds), 3)
  expect_equal(ds1[["B"]], ds[["B"]])
})

test_that("URLIST su un Dataset", {
  ds <- Dataset()
  ds["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), freq = 4)
  ds["B"] <- ts(c(0, 0, 0), start = c(1990, 1), freq = 12)

  ul <- URLIST(ds)
  expect_true(all(colnames(ul) %in% c("freq", "start", "end", "startp",
                                      "starty", "endp", "endy", "name")))

  aa <- ul[ul$name == "A",]
  expect_equal(aa$freq, 4)
  expect_equal(aa$starty, 1990)
  expect_equal(aa$start, 1990 + 1/4)
  expect_equal(aa$end, 1990+3/4)
  expect_equal(aa$startp, 1)
  expect_equal(aa$endy, 1990)
  expect_equal(aa$endp, 3)

  bb <- ul[ul$name == "B",]
  expect_equal(bb$freq, 12)
  expect_equal(bb$starty, 1990)
  expect_equal(bb$startp, 1)
  expect_equal(bb$start, 1990 + 1/12)
  expect_equal(bb$end, 1990+3/12)
  expect_equal(bb$endy, 1990)
  expect_equal(bb$endp, 3)
})



test_that("I can delete data from a Dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3))
  d["B"] <- ts(c(1, 2, 3))

  expect_equal(length(d), 2)
  expect_equal(names(d), c("A", "B"))

  d["A"] <- NULL

  expect_equal(length(d), 1)
  expect_true(!"A" %in% names(d))

  d[["B"]] <- NULL

  expect_equal(length(d), 0)
  expect_true(!"B" %in% names(d))

})


test_that("djoin joins each timeseries in Dataset", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["C"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["nonInD2"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)

  d2 <- Dataset()
  d2["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  d2["B"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  d2["C"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  d2["nonInD1"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 1)

  expect_warning(joined <- djoin(d1, d2, c(1990, 2)))
  expect_true(all(names(joined) %in% c("A","B","C", "nonInD1")))

  for(name in intersect(names(d2), names(d1))) {
    expect_equal(as.numeric(joined[[name]]), c(1, -2, -3))
  }
})

test_that("djoin fails with error if mergeSeries fails", {
  skip_if_not_installed("mockery")

  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["C"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["nonInD2"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 1)

  d2 <- Dataset()
  d2["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  d2["B"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  d2["C"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 4)
  d2["nonInD1"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 1)

  mockery::stub(.djoin, "tis::mergeSeries", function(...) stop("merge error"))
  expect_error(expect_warning(.djoin(d1, d2, c(1990, 2))), ": merge error")

})

test_that("merge merges each timeseries in Dataset", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["C"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["nonInD2"] <- ts(c(1, 2), start = c(1990, 1), frequency = 1)

  d2 <- Dataset()
  d2["A"] <- ts(c(-2, -3), start = c(1990, 2), frequency = 4)
  d2["B"] <- ts(c(-2, -3), start = c(1990, 2), frequency = 4)
  d2["C"] <- ts(c(-2, -3), start = c(1990, 2), frequency = 4)
  d2["nonInD1"] <- ts(c(-1, -2, -3), start = c(1990, 1), frequency = 1)

  expect_warning(joined <- merge(d1, d2), "non sono comuni")
  expect_true(all(names(joined) %in% c("A","B","C", "nonInD1")))
  for(name in intersect(names(d2), names(d1))) {
    expect_equal(as.numeric(joined[[name]]), c(1, -2, -3))
  }
})

test_that("You can have a union of Dataset", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d1["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)

  d2 <- Dataset()
  d2["B"] <- ts(c(-2, -3), start = c(1990, 2), frequency = 4)
  d2["C"] <- ts(c(-2, -3), start = c(1990, 2), frequency = 4)

  u <- union(d1, d2)
  expect_true(all(names(u) %in% c("A", "B", "C")))
  expect_equal(u[["B"]], d1[["B"]])
})

test_that("can copy a Dataset", {
  skip_if_not(require(pryr), "pryr is required")
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  d@url <- "fakeurl"

  x <- copy(d)
  for(name in names(x)) {
    expect_equal(x[[name]], d[[name]])
  }
  expect_equal(x@url, d@url)
  expect_true(address(x) != address(d))
})

test_that("you can round timeseries in Dataset", {
  skip_if_not(require(pryr), "pryr is required")
  d <- Dataset()
  d["A"] <- ts(c(1.001, 2.001, 3.001), start = c(1990, 1), frequency = 4)
  d["B"] <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)

  expect_error(round(d, digits="ciao"))
  x <- round(d, digits=1)
  expect_true(address(x) != address(d))
  expect_true(all(d[["A"]] - d[["B"]] != 0))
  expect_equal(x[["A"]], x[["B"]])
  expect_true(all(x[["A"]] - x[["B"]] == 0))
})

test_that("I can have an annual of a ts", {
  expected <- ts(12, start = 1990, frequency = 1)

  monthly <- ts(seq(12), start = c(1990, 1), frequency = 12)
  attr(monthly, "stock") <- 1
  expect_equal(annual(monthly), expected)

  monthly <- ts(rep(1, 12), start = c(1990, 1), frequency = 12)
  attr(monthly, "stock") <- 0
  expect_equal(annual(monthly), expected)

  quarterly <- ts(c(3,6,9, 12), start = c(1990, 1), frequency = 4)
  attr(quarterly, "stock") <- 1
  expect_equal(annual(quarterly), expected)

  quarterly <- ts(rep(3, 4), start = c(1990, 1), frequency = 4)
  attr(quarterly, "stock") <- 0
  expect_equal(annual(quarterly), expected)

  yearly <- ts(12, start = 1990, frequency = 1)
  expect_equal(yearly, expected)
  attr(yearly, "stock") <- 1
  expect_equal(as.numeric(annual(yearly)), as.numeric(expected))

  yearly <- ts(12, start = 1990, frequency = 1)
  attr(yearly, "stock") <- 0
  expect_equal(as.numeric(annual(yearly)), as.numeric(expected))
})

test_that("i can get an annual of an entire dataset", {
  d <- dataset()
  d["monthly"] <- {
    x <- ts(seq(12), start = c(1990, 1), frequency = 12)
    attr(x, "stock") <- 1
    x
  }
  d["quarterly"] <- {
    x <- ts(c(3, 6, 9, 12), start = c(1990, 1), frequency = 4)
    attr(x, "stock") <- 1
    x
  } 

  d["yearly"] <- {
    x <- ts(12, start = 1990, frequency = 1)
    attr(x, "stock") <- 1
    x
  }

  ann <- annual(d)
  for(name in names(ann)) {
    expect_equal(frequency(ann[[name]]), 1)
  }
})


test_that("I can sum series in a dataset", {
  ds <- Dataset()
  ds["A"] <- A <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["B"] <- B <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)

  expect_equal(sum(ds), ts(c(2,4,6), start = c(1990, 1), frequency = 4))
})

test_that("I get a warning if I sum different frequencies", {
  ds <- Dataset()
  ds["A"] <- A <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 4)
  ds["B"] <- B <- ts(c(1, 2, 3), start = c(1990, 1), frequency = 12)

  expect_warning(sum(ds), "has a different frequency")
})
