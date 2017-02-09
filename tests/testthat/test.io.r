context("Marshalling functions, ts->list, list->ts")

numbers <- (1:10)
year <- 1990
prd <- 1

tslist12 <- list(
  "numbers" = numbers,
  "year" = year,
  "period" = prd,
  "freq" = 12)

tss12 <- ts(numbers, start=c(year, prd), freq=12)

tslist4 <- list(
  "numbers" = numbers,
  "year" = year,
  "period" = prd,
  "freq" = 4)

tss4 <- ts(numbers, start=c(year, prd), freq=4)

test_that("to_list behaves like expected", {
  listed12 <- to_list(tss12)
  expect_equal(listed12$numbers, tslist12$numbers)
  expect_equal(listed12$freq, tslist12$freq)
  expect_equal(listed12$year, tslist12$year)
  expect_equal(listed12$period, tslist12$period)

  listed4 <- to_list(tss4)
  expect_equal(listed4$numbers, tslist4$numbers)
  expect_equal(listed4$freq, tslist4$freq)
  expect_equal(listed4$year, tslist4$year)
  expect_equal(listed4$period, tslist4$period)
})

test_that("from_list behaves like expected", {
  ts12 <- from_list(tslist12)
  expect_equal(ts12, tss12)
  expect_equal(frequency(ts12), frequency(tss12))

  ts4 <- from_list(tslist4)
  expect_equal(ts4, tss4)
  expect_equal(frequency(ts4), frequency(tss4))
})

test_that("to_list and from_list are symmetric", {
  expect_equal(from_list(to_list(tss12)), tss12)
  expect_equal(from_list(to_list(tss4)), tss4)

  l12 <- to_list(from_list(tslist12))
  expect_equal(sort(names(l12)), sort(names(tslist12)))

  for(name in names(l12)) {
    expect_equal(l12[[name]], tslist12[[name]])    
  }

  for(name in names(tslist12)) {
    expect_equal(tslist12[[name]], l12[[name]])
  }

  l4 <- to_list(from_list(tslist4))
  expect_equal(sort(names(l4)), sort(names(tslist4)))

  
  for(name in names(l4)) {
    expect_equal(l4[[name]], tslist4[[name]])    
  }

  for(name in names(tslist4)) {
    expect_equal(tslist4[[name]], l4[[name]])
  }
})

test_that("to_json produces a JSON string", {
  x <- to_json(tss12)
  expect_true(is.character(x))
})

test_that("from_json can create a timeseries", {
  json <- "{'freq':1, 'year':1990, 'period':1, 'numbers':[1,2,3,4]}"
  x <- from_json(json)
  expect_true(is.ts(x))
  expect_equal(frequency(x), 1)
  expect_equal(as.numeric(x), c(1,2,3,4))
  expect_equal(start(x), c(1990, 1))
}) 

test_that("from_json returns an array of numbers if it's not encoded as a timeseries", {
  json <- "[1,2,3,4]"
  x <- from_json(json)
  expect_true(is.numeric(x))
  expect_equal(x, c(1,2,3,4))
})

test_that("to_json can encode an array", {
  data <- c(1,2,3,4)
  x <- to_json(data)
  expect_equal(data, from_json(x))
})

test_that("from_list is able to convert data to the caller", {
  data <- c(1,2,3,4)
  raw_list <- list(numbers=data)
  expect_equal(from_list(raw_list), data)
})

test_that("from_list returns just the numbers if year, freq, period are not present", {
  data <- c(1,2,3,4)
  raw_list <- list(numbers=data)
  x <- from_list(raw_list)
  expect_equal(x, data)
  expect_true(!is.ts(x))

  raw_list[["year"]] <- 0
  x <- from_list(raw_list)
  expect_equal(x, data)
  expect_true(!is.ts(x))

  raw_list[["period"]] <- 0
  x <- from_list(raw_list)
  expect_equal(x, data)
  expect_true(!is.ts(x))

  raw_list[["freq"]] <- 0
  x <- from_list(raw_list)
  expect_equal(x, data)
  expect_true(!is.ts(x))
})
