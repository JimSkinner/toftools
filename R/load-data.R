#' Load ToF data
#'
#' This function takes the location of a ToF file (assumed to be stored as
#' molecules in columns and samples in rows), and returns a data frame.
#'
#' @param tof_file location of the ToF file
#' @return A data frame with samples in rows, molecules in columns.
#' @export
load_tof <- function(tof_file) {
  tof_data <- readr::read_delim(
    file = tof_file,
    delim = "\t",
    locale = readr::locale(encoding = "latin1"),
    skip_empty_rows = TRUE
  )

  tof_data_clean <- tof_data[-1,] %>% # Remove the top row containing the retention times
    tidyr::drop_na()                  # The last few rows are empty; drop these.

  # The name of the filename column is sometimes "JAC 2019". Change this to "sample".
  names(tof_data_clean)[1] <- "sample"

  return(tof_data_clean)
}

#' Extract two-class labels from a set of sample names
#'
#' Given a set of names, find the shortest prefix giving two unique classes (or
#' fail if no such prefix exists). Return a lenth n character vector containing
#' the classes.
#'
#' @param names A set of sample names
#' @return Character vector of labels
#' @export
extract_labels <- function(names) {
  names <- tolower(names)

  stopifnot(length(names) >= 2)
  stopifnot(length(unique(names)) > 1)

  max_length <- names %>%
    purrr::map_dbl(stringr::str_length) %>%
    min()

  for (end in 1:max_length) {
    labels <- names %>%
      stringr::str_sub(1, end)

    if (length(unique(labels)) == 2) {
      return(labels)
    } else if (length(unique(labels)) > 2) {
      stop(paste0(
        "Error: more than 2 classes found: ",
        paste0(unique(labels), collapse = ",")
      ))
    }
  }
}