#!/usr/bin/env Rscript

library(testthat)
library(methods)
library(hash)
library(doMC)
library(tools)
library(RJSONIO)

devtools::load_all()
auto_test("R", "tests/testthat/")
