#!/usr/bin/env Rscript

library(methods)
library(testthat)
library(hash)
library(tools)
library(doMC)
packrat::extlib("bimets")
devtools::load_all()
auto_test("R", "tests/testthat/")
