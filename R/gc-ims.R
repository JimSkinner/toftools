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

mat_to_vec <- function (gcims_mat) {
  stopifnot(is.matrix(gcims_mat))

  if (is.null(rownames(gcims_mat)))
    rownames(gcims_mat) <- paste0("GC", seq_len(nrow(gcims_mat)))

  if (is.null(colnames(gcims_mat)))
    colnames(gcims_mat) <- paste0("IMS", seq_len(ncol(gcims_mat)))

  vecnames <- toftools:::mat_names_to_vec(dimnames(gcims_mat))

  c(gcims_mat) %>%
    magrittr::set_names(vecnames)
}

read_cropped_gcims <- function(file, coarsen = 0) {
  df     <- (purrr::quietly(readr::read_delim)(file, ";", skip = 1))$result
  rownam <- as.character(df$`RT/DT`)
  colnam <- colnames(df)[-(1:2)]
  mat    <- as.matrix(df[,-(1:2)])
  dimnames(mat) <- list(GCtime = rownam, IMStime = colnam)

  if (coarsen > 0) {
    mat <- mat %>%
      toftools:::coarsen(size_GC = d_GC, size_IMS = d_IMS)
  }

  toftools:::mat_to_vec(mat)
}

#' Convert a GCIMS vector to a tbl
#'
#' Takes a GCIMS vector (vector with names giving the drift:retention times)
#' and returns a tbl with GC (retention) time, IMS (drift) time, and the value
#' given in the input vector.
#'
#' @param gcims_vec gcims vector
#' @return gcims tibble
#' @export
vec_to_tbl <- function(gcims_vec) {
  tibble::tibble(
    GC  = names(gcims_vec) %>%
      stringr::str_remove(":.*$") %>%
      stringr::str_remove_all("[a-zA-Z\\s]") %>%
      as.numeric(),
    IMS = names(gcims_vec) %>%
      stringr::str_remove("^.*:") %>%
      stringr::str_remove_all("[a-zA-Z\\s]") %>%
      as.numeric(),
    value = gcims_vec
  )
}

#' GCIMS vector to matrix
#'
#' Convert a GCIMS vector with informative column names into a GCIMS matrix
#'
#' @export
vec_to_mat <- function(gcims_vec) {
  stopifnot(is.vector(gcims_vec))
  stopifnot(!is.null(names(gcims_vec)))
  stopifnot(!any(duplicated(names(gcims_vec))))

  gcims_df <- toftools:::vec_to_tbl(gcims_vec)

  gcims_mat <- matrix(NA_real_,
                       nrow = length(unique(gcims_df$GC)),
                       ncol = length(unique(gcims_df$IMS)))

  dimnames(gcims_mat) <- list(
    GC  = gcims_df$GC %>%
      sort() %>%
      unique(),
    IMS = gcims_df$IMS %>%
      sort() %>%
      unique()
  )

  for (i in 1:nrow(gcims_df)) {
    gcims_mat[
      as.character(gcims_df$GC[i]),
      as.character(gcims_df$IMS[i])
      ] <- gcims_vec[i]
  }

  gcims_mat
}

gcims_feature_locations <- function(reference_mat, importance, discrete = FALSE) {
  importance$GC  = importance$feature %>% str_remove(":.*$") %>% as.numeric()
  importance$IMS = importance$feature %>% str_remove("^.*:") %>% as.numeric()

  plt <- plot_features(p_vals,
                       reference_mat,
                       n_keep,
                       TRUE,
                       ims_transformation = "log1p") +
    labs(title = str_glue("Machine {machine}, {n_keep} features"),
         colour = "p value")

        plt2 <- plot_features(p_vals,
                         reference_mat,
                         n_keep,
                         TRUE,
                         ims_transformation = "log1p",
                         discretise_features = TRUE) +
      labs(title = str_glue("Machine {machine}, {n_keep} features"),
           colour = "p value")

}