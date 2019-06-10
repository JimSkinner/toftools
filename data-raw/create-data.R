create_pigs_data <- function() {
  require(tidyverse)
  pigs <- load_tof("data-raw/pigs.txt")
  usethis::use_data(pigs)
}

create_coeliac_data <- function() {
  require(tidyverse)

  coeliac_height <- load_tof("data-raw/coeliac_height.txt")
  usethis::use_data(coeliac_height)

  coeliac_area <- load_tof("data-raw/coeliac_area.txt")
  usethis::use_data(coeliac_area)
}
