#!/usr/bin/env Rscript

library(methods)
library(testthat)
library(hash)
library(tools)
library(doMC)
devtools::load_all()
auto_test("R", "tests/testthat/")
