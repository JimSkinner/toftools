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