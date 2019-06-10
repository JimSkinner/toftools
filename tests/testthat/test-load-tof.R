context("load_tof")

test_that("load_tof functions as expected", {
  pigs <- load_tof("../../data-raw/pigs.txt")
  expect_equal(dim(pigs), c(281, 161))
})