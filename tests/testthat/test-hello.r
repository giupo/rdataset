context("Simple hello function")

test_that("hello function prints hello", {
  expect_message(hello(), "Hello")
})
