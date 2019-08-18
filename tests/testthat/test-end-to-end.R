context("system tests")

test_that("Calling 'analyse' on a TOF file produces a report without errors", {
  infile <- "../../data-raw/coeliac_height.txt"
  outfile <- "../../data-raw/coeliac_height.html"
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