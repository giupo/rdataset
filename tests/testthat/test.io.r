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
