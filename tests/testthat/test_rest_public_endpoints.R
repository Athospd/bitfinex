library(tibble)
library(dplyr)

context("bitfinex_candles")

test_that("bitfinex_candles", {
  expect_true(all(purrr::map_chr(frk_read(file_tiny), class) == types_tiny))
  expect_true(all(purrr::map_chr(frk_read(file_large), class) == types_large))
})

test_that("bitfinex_candles has conflict when lots of or all arguments are given", {

})

test_that("limit is 1000 lines", {

})

test_that("section argument works", {

})

test_that("aggr argument works", {

})

test_that("sort argument works", {

})

test_that("period argument works", {

})

test_that("start and end arguments works", {

})
