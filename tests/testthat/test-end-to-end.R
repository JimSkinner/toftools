context("system tests: TOF")

test_that("Calling 'analyse' on a TOF file (coeliac_height) produces a report without errors", {
  infile <- "../../data-raw/coeliac_height.txt"
  outfile <- "../../data-raw/coeliac_height.html"

  suppressWarnings(file.remove(outfile)) # Existing file can hide failed test

  analyse(infile)

  expect_true(file.exists(outfile))
  file.remove(outfile) # Clean up
})

test_that("Calling 'analyse' on a TOF file (sepsis_area) produces a report without errors", {
  infile <- "../../data-raw/sepsis_area.txt"
  outfile <- "../../data-raw/sepsis_area.html"

  suppressWarnings(file.remove(outfile)) # Existing file can hide failed test

  analyse(infile)

  expect_true(file.exists(outfile))
  file.remove(outfile) # Clean up
})

test_that("Calling 'analyse' on a TOF file with >2 classes produces an error", {
  infile <- "../../data-raw/pigs.txt"
  outfile <- "../../data-raw/pigs.html"

  suppressWarnings(file.remove(outfile)) # Existing file can make successful test appear failed

  expect_error(analyse(infile))
  expect_false(file.exists(outfile))
})

context("system tests: GCIMS")

test_that("Calling 'analyse_GCIMS' on a directory of GC-IMS files produces a report without errors", {
  indir <- "../../data-raw/gc ims" # Checks that spaces in directory names are supported
  outfile <- "../../data-raw/gc ims/gcims-analysis.html"

  suppressWarnings(file.remove(outfile)) # Existing file can hide failed test

  analyse_GCIMS(indir)

  expect_true(file.exists(outfile))
  file.remove(outfile) # Clean up
})

