context("system tests: TOF")

test_that("Calling 'analyse' on a TOF file (coeliac_height) produces a report without errors", {
  infile <- "../../data-raw/coeliac_height.txt"
  outfile <- "../../data-raw/coeliac_height.html"
  analyse(infile)

  expect_true(file.exists(outfile))
  file.remove(outfile)
})

test_that("Calling 'analyse' on a TOF file (sepsis_area) produces a report without errors", {
  infile <- "../../data-raw/sepsis_area.txt"
  outfile <- "../../data-raw/sepsis_area.html"
  analyse(infile)

  expect_true(file.exists(outfile))
  file.remove(outfile)
})

test_that("Calling 'analyse' on a TOF file with >2 classes produces an error", {
  infile <- "../../data-raw/pigs.txt"
  outfile <- "../../data-raw/pigs.html"
  expect_error(analyse(infile))
  expect_false(file.exists(outfile))
})