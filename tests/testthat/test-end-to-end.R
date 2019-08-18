context("system tests")

test_that("Calling 'analyse' on a TOF file produces a report without errors", {
  infile <- "../../data-raw/coeliac_height.txt"
  outfile <- "../../data-raw/coeliac_height.html"
  analyse(infile)

  expect_true(file.exists(outfile))
  file.remove(outfile)
})