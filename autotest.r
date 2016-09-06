#!/usr/bin/env Rscript

library(methods)
library(testthat)

devtools::load_all()
auto_test("R", "tests/testthat/")
