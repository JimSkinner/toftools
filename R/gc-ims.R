coarsen <- function(mat, size_GC = 100, size_IMS = 100) {
  stopifnot(!is.null(dimnames(mat)))

  interpolate_names <- function(names, npoints) {
    namerange <- names %>% as.numeric() %>% range()
    seq(namerange[1], namerange[2], length.out = npoints) %>%
      round(3) %>% as.character()
  }

  olddimnames <- dimnames(mat)
  newdimnames <- olddimnames
  newdimnames[[1]] <- interpolate_names(olddimnames[[1]], size_GC)
  newdimnames[[2]] <- interpolate_names(olddimnames[[2]], size_IMS)
  outmat <- mat %>%
    imager::as.cimg() %>%
    imager::resize(size_x = size_GC,
                   size_y = size_IMS,
                   interpolation_type = 2) %>%
    as.matrix()

  dimnames(outmat) <- newdimnames
  outmat
}

mat_names_to_vec <- function (dimnames) {
  rownames <- dimnames[[1]]
  colnames <- dimnames[[2]]
  name_matrix <- outer(rownames, colnames, function(r, c) {
    paste0(r, ":", c)
  })
  c(name_matrix)
}

mat_to_vec <- function (breath_mat) {
  stopifnot(is.matrix(breath_mat))

  if (is.null(rownames(breath_mat)))
    rownames(breath_mat) <- paste0("GC", seq_len(nrow(breath_mat)))

  if (is.null(colnames(breath_mat)))
    colnames(breath_mat) <- paste0("IMS", seq_len(ncol(breath_mat)))

  vecnames <- toftools::mat_names_to_vec(dimnames(breath_mat))

  c(breath_mat) %>%
    magrittr::set_names(vecnames)
}

read_cropped_breath <- function(file, coarsen = 0) {
  df     <- (purrr::quietly(readr::read_delim)(file, ";", skip = 1))$result
  rownam <- as.character(df$`RT/DT`)
  colnam <- colnames(df)[-(1:2)]
  mat    <- as.matrix(df[,-(1:2)])
  dimnames(mat) <- list(GCtime = rownam, IMStime = colnam)

  if (coarsen > 0) {
    mat <- mat %>%
      toftools::coarsen(size_GC = d_GC, size_IMS = d_IMS)
  }

  toftools::mat_to_vec(mat)
}
