context("Data Structures, Dataset")

library(zoo)

.setUp <- function() {  # noqa  
  workdir <<- rutils::workDir()
  jsonFilePath <<- system.file(package="rdataset", "test.json")
  spk_lib <<- rutils::tempdir()
  spk_list <<- file.path(spk_lib, "test.list")

  write("test", file.path(spk_lib, "test.keep"))
  write("test", spk_list)
}

.tearDown <- function() {
  unlink(spk_lib, recursive=TRUE, force=TRUE)
  # unlink(workdir, recursive=TRUE, force=TRUE)
}

test_that("is_JSON behaves correctly", {
  .setUp()
  expect_true(is_JSON(jsonFilePath))
  expect_error(is_JSON("/non/esisto/dir/dir/pluto.json"))
  expect_true(is_JSON(workdir) == FALSE)
  expect_true(is_JSON("~")== FALSE)
  .tearDown()
})

test_that("load.dataset works as expected", {
  .setUp()
  expect_error(dataset("http://localhost"))
  .tearDown()
})

test_that("slicing works as expected", {
  .setUp()
  d <- suppressWarnings(Dataset())

  tt <- ts(rep(0,10), start=c(1990,1), freq=4)
  d["TEST"] <- tt
  expect_true("TEST" %in% names(d))
  d[["TEST2"]] <- tt
  expect_true("TEST2" %in% names(d))
  .tearDown()
})

test_that("slicing with multiple names", {
  .setUp()
  data <- list()
  data[["A"]] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  data[["B"]] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  d <- as.dataset(data)
  d[[names(data)]] <- data
  expect_true(all(c("A", "B") %in% names(d)))
  .tearDown()
})

test_that("as.dataset works with list, characters", {
  .setUp()
  l <- list(
    A=ts(c(1,2,3), start=c(1990,1), freq=4),
    B=ts(c(1,2,3), start=c(1990,1), freq=4))
  
  ds <- as.dataset(l)
  expect_true(is.dataset(ds))
  expect_true(all(c("A", "B") %in% names(ds)))
  .tearDown()
})

test_that("boolean operators work on dataset", {
  ds <- Dataset()
  ds["A"] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  ds["B"] <- ts(c(0,0,0), start=c(1990,1), freq=4)
  ds["C"] <- ds[["B"]] - ds[["A"]]
  out <- ds[ds == 0]
  expect_true("B" %in% names(out))
  out <- ds[ds > 0]
  expect_true("A" %in% names(out))
  out <- ds[ds < 0.1]
  expect_true("C" %in% names(out))
})

test_that("I can diff two datasets", {
  ds1 <- Dataset()
  ds2 <- Dataset()

  ds1["A"] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  ds2["A"] <- ts(c(1,2,3), start=c(1990,1), freq=4)

  diff <- ds1 - ds2
  expect_true("A" %in% names(diff))
  expect_equal(sum(abs(diff[["A"]])),0)
})

test_that("I can use or omit which when subsetting", {
  ds <- Dataset()
  ds["A"] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  ds["B"] <- ts(c(0,0,0), start=c(1990,1), freq=4)
  ds["C"] <- ds[["B"]] - ds[["A"]]
  out1 <- ds[ds == 0]
  out2 <- ds[which(ds == 0)]
  expect_true("B" %in% names(out1))
  expect_true("B" %in% names(out2))    
})

test_that("I can use abs function over a Dataset", {
  ds <- Dataset()
  ds["A"] <- ts(c(-1,-2,-3), start=c(1990,1), freq=4)
  ds["B"] <- ts(c(0,0,0), start=c(1990,1), freq=4)
  # ads <- abs(ds)
  # expect_true(all(ads[["A"]] > 0))

  # patch per ticket: 31922
  ads <- abs_ds(ds)
  expect_true(all(ads[["A"]] > 0))
})

test_that("as list on datasets works as expected", {
  ds <- Dataset()
  ds["A"] <- ts(c(-1, -2, -3), start = c(1990, 1), freq = 4)
  ds["B"] <- ts(c(0, 0, 0), start = c(1990, 1), freq = 4)
  l <- as.list(ds)
  expect_true(is.list(l))
  expect_true(all(unlist(lapply(l, is.ts))))
  expect_true(!all(unlist(lapply(l, is.dataset))))
  expect_true("A" %in% names(l))
  expect_true("B" %in% names(l))
  expect_true(all(names(l) %in% names(ds)))
  expect_equal(ds[["A"]], l$A)
  expect_equal(ds[["B"]], l$B)
})

test_that("Union of datasets works as expected", {
  ds1 <- Dataset()
  ds1["A"] <- ts(c(-1,-2,-3), start=c(1990,1), freq=4)
  ds1["B"] <- ts(c(0,0,0), start=c(1990,1), freq=4)
  ds2 <- Dataset()
  ds2["C"] <- ts(c(-1,-2,-3), start=c(1990,1), freq=4)
  ds2["D"] <- ts(c(0,0,0), start=c(1990,1), freq=4)

  ds <- union(ds1, ds2)
  
  expect_true(all(names(ds1) %in% names(ds)))
  expect_true(all(names(ds2) %in% names(ds)))

  expect_true(all(unlist(lapply(as.list(ds), is.ts))))
  expect_true(!all(unlist(lapply(as.list(ds), is.dataset))))
})

test_that("union of datasets behaves like a regular union", {
  ds1 <- Dataset()
  ds1["A"] <- ts(c(-1,-2,-3), start=c(1990,1), freq=4)
  ds1["B"] <- ts(c(0,0,0), start=c(1990,1), freq=4)

  ds2 <- Dataset()
  ds2["B"] <- ts(c(-1,-2,-3), start=c(1990,1), freq=4)
  ds2["D"] <- ts(c(0,0,0), start=c(1990,1), freq=4)

  ds <- union(ds1, ds2)

  expect_equal(length(ds), 3)
  expect_equal(ds1[["B"]], ds[["B"]])
})

test_that("URLIST su un Dataset", {
  ds <- Dataset()
  ds["A"] <- ts(c(-1, -2, -3), start = c(1990,1), freq = 4)
  ds["B"] <- ts(c(0, 0, 0), start = c(1990,1), freq = 12)

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

test_that("Dataset fails with error if it doesn't know how to handle url", {
  expect_error(Dataset("http://localhost"))
})

test_that("failig to match subsetting returns an empty Dataset", {
  .setUp()
  d <- as.dataset(list(A=1))
  idx <- rep(FALSE, length(d))
  expect_equal(length(d[idx]), 0)
  .tearDown()
})

test_that("subsetting with a zero numeric array returns an empty Dataset", {
  .setUp()
  d <- Dataset()
  idx <- numeric(0)
  expect_equal(length(d[idx]), 0)
  .tearDown()
})

test_that("subsetting with inconsistent numeric array returns an error", {
  .setUp()
  d <- Dataset()
  idx <- c(8,9,10)
  expect_error(d[idx])
  .tearDown()
})

test_that("subsetting with inconsistent char array returns a warning and an empty Dataset", {
  .setUp()
  d <- Dataset()
  idx <- c("A", "B", "C")
  expect_warning(expect_equal(length(d[idx]), 0))
  .tearDown()
})

test_that("Setting to null an object removes it from Dataset", {
  .setUp()
  d <- as.dataset(list(TS1=1))
  origLength <- length(d)
  d["TS1"] <- NULL
  expect_equal(length(d), origLength - 1)
  .tearDown()
})


test_that("I can produce an xlsx from a Dataset", {
  skip("this test fails without reason on Windows")
  ds <- Dataset()
  if(!suppressWarnings(require(xlsx))) {
    skip("Can't run this test without xlsx")
  }
  ds["TS1"] <- ts(c(1,2,3), start=c(1990,1), freq=1)
  ds["TS4"] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  ds["TS12"] <- ts(c(1,2,3), start=c(1990,1), freq=12)
  ds["TS1-1"] <- ts(c(1,2,3), start=c(1990,1), freq=1)
  ds["TS4-1"] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  ds["TS12-1"] <- ts(c(1,2,3), start=c(1990,1), freq=12)
  expect_true(require(rprogressbar))
  tmpfile <- tempfile(fileext=".xlsx")
  # tmpfile <- "~/tmp.xlsx"
  on.exit(unlink(tmpfile, force=TRUE))
  to_xlsx(ds, tmpfile)
  expect_true(file.info(tmpfile)$size > 0)
})

test_that("is_csv_library works as expected", {
  directory <- system.file(package="rdataset", "csvdataset")
  expect_true(file.info(directory)$isdir)
  expect_true(is_csv_library(directory))
})

test_that("Init with a non existing directory raises a warning and an error", {
  expect_error(expect_warning(Dataset("/io/non/esisto")))
})

test_that("calling with an unsupported protocol raises an error", {
  expect_warning(expect_error(Dataset("ftp://helloWorld")))
})

test_that("can't subset with list()", {
  d <- Dataset()
  expect_error(d[list(a=1)], "can't subset")
})

test_that("I can delete data from a Dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))

  expect_equal(length(d), 2)
  expect_equal(names(d), c("A", "B"))

  d["A"] <- NULL

  expect_equal(length(d), 1)
  expect_true(!"A" %in% names(d))

  d[["B"]] <- NULL

  expect_equal(length(d), 0)
  expect_true(!"B" %in% names(d))

})

test_that("`as.vector` returns a list representtion of this Dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))
  expect_is(as.vector(d), "list")
  expect_equal(as.vector(d), list(A=d[["A"]], B=d[["B"]]))
})

test_that("differences from a dataset with different names raises a warning", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1,2,3))
  d1["B"] <- ts(c(1,2,3))
  d2 <- Dataset()
  d2["A"] <- ts(c(1,2,3))
  d2["B"] <- ts(c(1,2,3))
  d2["C"] <- ts(c(1,2,3))

  expect_warning(d1-d2)
  expect_warning(d2-d1)
})


test_that("differences from a dataset with zero common names raises an error", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1,2,3))
  d1["B"] <- ts(c(1,2,3))
  d2 <- Dataset()
  d2["C"] <- ts(c(1,2,3))
  d2["D"] <- ts(c(1,2,3))

  expect_error(d1-d2)
})

test_that("comparators return a logical values", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1,2,3))
  d1["B"] <- ts(c(1,2,3))

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

test_that("Expect output from show", {
  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))
  d@url <- "http://blabla"
  expect_output(show(d), "Dataset ")
  expect_output(show(d), "http://blabla")
})

test_that("saveDataset behaves like expected", {
  skip_if_not(require(mockery), "mockery required")
  output <- tempfile()
  on.exit(file.remove(output))

  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))

  expect_error(saveDataset(d, output), NA)
})


test_that("fullsummary e shortsummary provide an output", {
  d <- Dataset()
  d["A"] <- ts(c(1,2,3), start=c(1990,1), frequency=1)
  d["B"] <- ts(c(1,2,3), start=c(1990,1), frequency=4)
  d["C"] <- ts(c(1,2,3), start=c(1990,1), frequency=12)
  # to test freq bins...
  d["A1"] <- ts(c(1,2,3), start=c(1990,1), frequency=1)
  d["B1"] <- ts(c(1,2,3), start=c(1990,1), frequency=4)
  d["C1"] <- ts(c(1,2,3), start=c(1990,1), frequency=12)
  
  expect_output(shortSummary(d))
  expect_output(fullSummary(d))  
})

test_that("as.dataset on a dataset returns a dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))

  x <- as.dataset(d)
  expect_true(is.dataset(x))
  expect_equal(names(d), names(x))
  for(name in names(d)) {
    expect_equal(d[[name]], x[[name]])
  }
})

test_that("djoin joins each timeseries in Dataset", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1,2,3), start=c(1990,1), frequency=4)
  d1["B"] <- ts(c(1,2,3), start=c(1990,1), frequency=4)
  d1["C"] <- ts(c(1,2,3), start=c(1990,1), frequency=4)
  d1["nonInD2"] <- ts(c(1,2,3), start=c(1990,1), frequency=1)
  
  d2 <- Dataset()
  d2["A"] <- ts(c(-1,-2,-3), start=c(1990,1), frequency=4)
  d2["B"] <- ts(c(-1,-2,-3), start=c(1990,1), frequency=4)
  d2["C"] <- ts(c(-1,-2,-3), start=c(1990,1), frequency=4)
  d2["nonInD1"] <- ts(c(-1,-2,-3), start=c(1990,1), frequency=1)

  expect_warning(joined <- djoin(d1, d2, c(1990,2)))
  expect_true(all(names(joined) %in% c("A","B","C", "nonInD1")))
  
  for(name in intersect(names(d2), names(d1))) {
    expect_equal(as.numeric(joined[[name]]), c(1, -2, -3))
  }
  
  with_mock(
    'tis::mergeSeries' = function(...) stop("error"), {
      expect_error(expect_warning(djoin(d1, d2, c(1990,2))), ": error")
    })
    
})

test_that("merge merges each timeseries in Dataset", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  d1["B"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  d1["C"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  d1["nonInD2"] <- ts(c(1,2), start=c(1990,1), frequency=1)
  
  d2 <- Dataset()
  d2["A"] <- ts(c(-2,-3), start=c(1990,2), frequency=4)
  d2["B"] <- ts(c(-2,-3), start=c(1990,2), frequency=4)
  d2["C"] <- ts(c(-2,-3), start=c(1990,2), frequency=4)
  d2["nonInD1"] <- ts(c(-1,-2,-3), start=c(1990,1), frequency=1)
  
  expect_warning(joined <- merge(d1, d2), "non sono comuni")
  expect_true(all(names(joined) %in% c("A","B","C", "nonInD1")))
  for(name in intersect(names(d2), names(d1))) {
    expect_equal(as.numeric(joined[[name]]), c(1, -2, -3))
  }
})

test_that("You can have a union of Dataset", {
  d1 <- Dataset()
  d1["A"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  d1["B"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
   
  d2 <- Dataset()
  d2["B"] <- ts(c(-2,-3), start=c(1990,2), frequency=4)
  d2["C"] <- ts(c(-2,-3), start=c(1990,2), frequency=4)

  u <- union(d1, d2)
  expect_true(all(names(u) %in% c("A", "B", "C")))
  expect_equal(u[["B"]], d1[["B"]])
})

test_that("can copy a Dataset", {
  skip_if_not(require(pryr), "pryr is required")
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  d["B"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
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
  d["A"] <- ts(c(1.001, 2.001, 3.001), start=c(1990,1), frequency=4)
  d["B"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)

  expect_error(round(d, digits="ciao"))
  x <- round(d, digits=1)
  expect_true(address(x) != address(d))
  expect_true(all(d[["A"]] - d[["B"]] != 0))
  expect_equal(x[["A"]], x[["B"]])
  expect_true(all(x[["A"]] - x[["B"]] == 0))
})

test_that("I can have an annual of a ts", {
  expected <- ts(12, start=1990, frequency=1)

  monthly <- ts(seq(12), start=c(1990,1), frequency=12)
  attr(monthly, "stock") <- 1
  expect_equal(annual(monthly), expected)

  monthly <- ts(rep(1, 12), start=c(1990,1), frequency=12)
  attr(monthly, "stock") <- 0
  expect_equal(annual(monthly), expected)

  quarterly <- ts(c(3,6,9,12), start=c(1990,1), frequency=4)
  attr(quarterly, "stock") <- 1
  expect_equal(annual(quarterly), expected)

  quarterly <- ts(rep(3, 4), start=c(1990,1), frequency=4)
  attr(quarterly, "stock") <- 0
  expect_equal(annual(quarterly), expected)

  yearly <- ts(12, start=1990, frequency=1)
  expect_equal(yearly, expected)
  attr(yearly, "stock") <- 1
  expect_equal(as.numeric(annual(yearly)), as.numeric(expected))

  yearly <- ts(12, start=1990, frequency=1)
  attr(yearly, "stock") <- 0
  expect_equal(as.numeric(annual(yearly)), as.numeric(expected))
})

test_that('i can get an annual of an entire dataset', {
  d <- dataset()
  d["monthly"] <- {
    x <- ts(seq(12), start=c(1990,1), frequency=12)
    attr(x, "stock") <- 1
    x
  }
  d["quarterly"] <- {
    x <- ts(c(3,6,9,12), start=c(1990,1), frequency=4)
    attr(x, "stock") <- 1
    x
  } 

  d["yearly"] <- {
    x <- ts(12, start=1990, frequency=1)
    attr(x, "stock") <- 1
    x
  }

  ann <- annual(d)
  for(name in names(ann)) {
    expect_equal(frequency(ann[[name]]), 1)
  }
})

test_that("I can access series with $", {
   ds <- Dataset()
   ds["A"] <- A <- ts(c(1,2,3), start=c(1990,1), freq=1)
   ds["B"] <- B <- ts(c(1,2,3), start=c(1990,1), freq=4)
   expect_equal(ds$A, A)
   expect_equal(ds$B, B)
   expect_equal(ds$`A B`, ds[[c("A", "B")]])
})
