context("Data Structures, Dataset")

library(zoo)

.setUp <- function() {  # noqa  
  workdir <<- rutils::workDir()
  # jsonFilePath <<- file.path(workdir, "pippo.json")
  # write("test", jsonFilePath)

  jsonFilePath <<- system.file(package="rdataset", "test.json")
  dir_grafo <<- rutils::tempdir()
  spk_lib <<- rutils::tempdir()
  spk_list <<- file.path(spk_lib, "test.list")
  suppressWarnings({
    dir.create(file.path(dir_grafo, "tasks"), recursive = TRUE)
    dir.create(file.path(dir_grafo, "data", "T"), recursive = TRUE)
    dir.create(file.path(dir_grafo, "metadati"), recursive = TRUE)
    dir.create(spk_lib, recursive=TRUE)
  })

  write("test", file.path(spk_lib, "test.keep"))
  write("test", spk_list)

  write("1990\n1\n1\n0\n0\n0", file.path(dir_grafo, "data", "T", "TS1.csv"))
  write("1990\n1\n1\n1\n1\n1", file.path(dir_grafo, "data", "T", "TS2.csv"))
}

.tearDown <- function() {
  unlink(dir_grafo, recursive=TRUE, force=TRUE)
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

test_that("is_grafo works as expected",  {
  .setUp()
  expect_true(is_grafo(dir_grafo))
  expect_true(is_grafo(workdir) == FALSE)
  .tearDown()
})

test_that("load.dataset works as expected", {
  .setUp()
  d <- suppressWarnings(Dataset(dir_grafo))
  expect_true(is.dataset(d["TS1"]))
  expect_true(length(d) == 2)
  expect_error(dataset("http://localhost"))
  .tearDown()
})

test_that("slicing works as expected", {
  .setUp()
  d <- suppressWarnings(Dataset(dir_grafo))
  expect_true(is.ts(d[["TS1"]]))

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
  d <- suppressWarnings(Dataset(dir_grafo))
  data[["A"]] <- ts(c(1,2,3), start=c(1990,1), freq=4)
  data[["B"]] <- ts(c(1,2,3), start=c(1990,1), freq=4)

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
  
  ds <- suppressWarnings(as.dataset(dir_grafo))
  expect_true(is.dataset(ds))
  expect_true(all(c("TS1", "TS2") %in% names(ds)))
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
  ds <- abs(ds)
  expect_true(all(ds[["A"]] > 0))
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

  ds <- union.Dataset(ds1, ds2)
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
  ds <- union.Dataset(ds1, ds2)
  expect_equal(length(ds), 3)
  expect_equal(ds[["B"]], ds1[["B"]])
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
  d <- Dataset(dir_grafo)
  idx <- rep(FALSE, length(d))
  expect_equal(length(d[idx]), 0)
  .tearDown()
})

test_that("subsetting with a zero numeric array returns an empty Dataset", {
  .setUp()
  d <- Dataset(dir_grafo)
  idx <- numeric(0)
  expect_equal(length(d[idx]), 0)
  .tearDown()
})

test_that("subsetting with inconsistent numeric array returns an error", {
  .setUp()
  d <- Dataset(dir_grafo)
  idx <- c(8,9,10)
  expect_error(d[idx])
  .tearDown()
})

test_that("subsetting with inconsistent char array returns a warning and an empty Dataset", {
  .setUp()
  d <- Dataset(dir_grafo)
  idx <- c("A", "B", "C")
  expect_warning(expect_equal(length(d[idx]), 0))
  .tearDown()
})

test_that("Setting to null an object removes it from Dataset", {
  .setUp()
  d <- Dataset(dir_grafo)
  origLength <- length(d)
  d["TS1"] <- NULL
  expect_equal(length(d), origLength - 1)
  .tearDown()
})

test_that("I can load datasets of timeseries", {
  .setUp()
  d <- Dataset(jsonFilePath)
  .tearDown()
})

test_that("I can produce an xlsx from a Dataset", {
  ds <- Dataset()
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

test_that("a dataset can be created from a CSV-filled directory", {
  directory <- system.file(package="rdataset", "csvdataset")
  expect_true(file.info(directory)$isdir)
  d <- Dataset(directory)
  expect_true(length(d) > 0)
  for(nome in names(d)) {
    expect_true(is.ts(d[[nome]]))
  }

  d <- Dataset(directory, ids=c("A"))
  expect_true("A" %in% names(d))
  expect_true(!"B" %in% names(d))
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

test_that("`as.vector` is equal to `names` on a Dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))
  expect_equal(names(d), as.vector(d))
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
  called <- 0
  d <- Dataset()
  d["A"] <- ts(c(1,2,3))
  d["B"] <- ts(c(1,2,3))

  with_mock(
    `base::write`=function(...) { called <<- called + 1 }, {
      saveDataset(d, "/non/esisto", as.csv = FALSE, as.grafo=FALSE)
      expect_true(called > 0)
      called <<- 0
    })

   with_mock(
    `rdataset::tsWrite_nativo`=function(...) { called <<- called + 1 }, {
      saveDataset(d, "/non/esisto", as.csv = TRUE)
      expect_true(called > 0)
      called <<- 0
    })

})

test_that("tsWrite_nativo writes a timeseries as CSV", {
  tmpdir <- rutils::tempdir()
  on.exit(unlink(tmpdir, recursive=TRUE, force=TRUE))
  path <- file.path(tmpdir, "/test.csv")
  x <- ts(c(1,2,3,4), start=c(1990,1), frequency=4)
  tsWrite_nativo(x, path)
  linee <- rutils::readLines(path)
  expected <- c(1990,1,4,1,2,3,4)
  for(i in seq_along(linee)) {
    token <- linee[[i]]
    expect_equal(as.numeric(token), expected[[i]])
  }


  x <- ts(c(1,2,3,4), start=c(1990,4), frequency=4)
  tsWrite_nativo(x, path)
  linee <- rutils::readLines(path)
  expected <- c(1990,4,4,1,2,3,4)
  for(i in seq_along(linee)) {
    token <- linee[[i]]
    expect_equal(as.numeric(token), expected[[i]])
  }
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

test_that("to_csv creates a string CSV of the Dataset", {
  d <- Dataset()
  d["A"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  d["B"] <- ts(c(1, 2, 3), start=c(1990,1), frequency=4)
  x <- to_csv(d)
  expect_true(is.character(x))
})

test_that("can copy a Dataset", {
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

