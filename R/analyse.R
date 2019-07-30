#' Produce an html analysis of a ToF file
#'
#' Given a ToF file, train a variety of classifiers & look at cross-validation
#' accuracy and feature importance. Classes are determined by the first letter
#' of each sample name.
#' @param tof_file location of the ToF file
#' @param tune (optional logical, default FALSE) whether to tune hyper-parameters.
#'   If TRUE, will take a LOT more compute time.
#' @export
analyse <- function(tof_file, tune = FALSE) {
  tof_file <- normalizePath(tof_file)

  if (!file.exists(tof_file)) {
    stop(paste0("File does not exist: ", tof_file))
  }

  template <- system.file("Rmd", "analysis-template.Rmd", package = "toftools")

  output <- tof_file %>%
    tools::file_path_sans_ext() %>%
    paste0(".html")

  rmarkdown::render(
    input = template,
    output_file = basename(output),
    output_dir  = dirname(output),
    params = list(
      tof_file = tof_file,
      tune     = tune
    )
  )
}

#' Run 'analyse' on every '.txt' file in a directory
#'
#' @param tof_dir directory containing ToF files (names ending in .txt)
#' @param pattern The filename patterns to match (default '*.txt')
#' @param tune (optional logical, default FALSE) whether to tune hyper-parameters.
#'   If TRUE, will take a LOT more compute time.
#' @return a list of error messages; one per file.
#' @export
analyse_dir <- function(tof_dir, pattern = "*.txt", tune = FALSE) {
  list.files(tof_dir, pattern = pattern, full.names = TRUE) %>%
    purrr::map(~ purrr::safely(purrr::partial(analyse, tune = tune))(.x)$error)
}

#' Produce a report on the predictive accuracy of a directory of pre-processed
#' GC-IMS data.
#'
#' @param dir directory containing pre-processed GC-IMS files
#' @param pattern (optional string, default '*.csv') File extension of GC-IMS files
#' @export
analyse_GCIMS <- function(dir, pattern = "*.csv") {

  ## TODO: Interactive dir chooser

  files <- list.files(dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop(stringr::str_glue("No files matching pattern ({pattern})"))
  }

  ## Build class labels using 1st letter of filenames (expect 2 unique)
  label <- basename(files) %>%
    stringr::str_extract("[a-zA-Z]") %>%  # Take out 1st letter in the filename
    as.factor()                           # Classes in alphabetic order

  if (length(unique(label)) != 2) {
    stop(stringr::str_glue("Expected 2 unique 1st letter file names. Found: {length(unique(label)) != 2}"))
  }

  ## Load all GCIMS data into a matrix

  # TODO: Print a good error message if it is not a pre-processed GCIMS file.

  prototype <- read_cropped_gcims(files[[1]])
  d <- length(prototype)

  gcims_mat <- files %>%
    vapply(read_cropped_gcims, numeric(d)) %>%
    t()

  rownames(gcims_mat) <- basename(files)

  ## Pre-process the data matrix
  # TODO: Remove low IQR, etc..

  ## Render the gcims analysis template (pass matrix & labels as params)
  template <- system.file("Rmd", "analysis-template-gcims.Rmd", package = "toftools")
  rmarkdown::render(
    input = template,
    output_file = "gcims-analysis.html",
    output_dir  = dir,
    params = list(
      X = gcims_mat,
      y = label
    )
  )
}