data(pigs)

iqrs <- pigs[,-1] %>%
  dplyr::summarise_all(IQR) %>%
  tidyr::gather(key = "molecule", value = "quantity", dplyr::everything()) %>%
  dplyr::mutate(order = order(quantity))

keep_vars <- iqrs %>%
  dplyr::filter(order < 50) %>%
  dplyr::pull(molecule)

pigs <- pigs %>%
  dplyr::select_at(c("filename", keep_vars))
