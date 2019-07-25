#' Plot one or more GC-IMS matrices
#' @param gcims A 'gcims' matrix, or a list of such matrices
#' @param subsample (integer) Sub-sample 1-in-`subsample' pixels
#' @return ggplot object plotting the matrices
#' @export
plot_gcims <- function(gcims, names = character(0), subsample=1, max_signal=Inf, nrow = NULL, ncol = NULL, ims_transformation = "identity") {
  if (is.list(gcims)) {
    if (length(names) > 1) {
      stopifnot(length(names) == length(gcims))
      names(gcims) <- names
    } else if (!is.null(names(gcims))) {
      names(gcims) <- basename(names(gcims))
    }
  } else if (is.matrix(gcims)) {
    gcims <- list(gcims)
  } else {
    stop("'gcims' should be either a matrix or a list of matrices")
  }

  # Check all inputs have row and column
  if (any(purrr::map_lgl(gcims, ~ is.null(rownames(.x))))) {
    stop("All gcims inputs must have row names")
  }
  if (any(purrr::map_lgl(gcims, ~ is.null(colnames(.x))))) {
    stop("All gcims inputs must have column names")
  }

  # 'Smooth' gcims columns names so that there are no gaps in the raster
  gcims <- gcims %>% purrr::map(function(mat) {
    oldnames <- as.numeric(colnames(mat))
    colnames(mat) <- seq(min(oldnames), max(oldnames), length = length(oldnames))
    mat
  })

  # Same for rows
  gcims <- gcims %>% purrr::map(function(mat) {
    oldnames <- as.numeric(rownames(mat))
    rownames(mat) <- seq(min(oldnames), max(oldnames), length = length(oldnames))
    mat
  })

  # The sample with the greatest nunber of rows
  biggestgcims <- gcims[[gcims %>% purrr::map_int(base::nrow) %>% which.max()]]

  # Rows and columns to keep, according to "subsample"
  keeprownames <- rownames(biggestgcims)[(seq_len(nrow(biggestgcims))%%subsample)==0]
  keepcolnames <- colnames(biggestgcims)[(seq_len(ncol(biggestgcims))%%subsample)==0]

  # Tidy
  gcims <- gcims %>% purrr::map(
    function(b) `[`(b,
                    intersect(rownames(b), keeprownames),
                    intersect(colnames(b), keepcolnames),
                    drop = FALSE) %>%
      reshape2::melt() %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(as.numeric) %>%
      rlang::set_names(c("GC.runtime", "IMS.drift.time", "IMS.signal"))
  )

  # 'sample' column only added if list has length > 1
  if (length(gcims) > 1) {
    gcims <- gcims %>%
      dplyr::bind_rows(.id='samplename') %>%
      dplyr::mutate(samplename = factor(samplename, levels=names(gcims)))
  } else {
    gcims <- gcims[[1]]
  }

  ## Apply signal cap --------
  gcims$IMS.signal <- pmin(gcims$IMS.signal, max_signal)

  ## Make the plot ----------
  plt <- ggplot2::ggplot(gcims, ggplot2::aes(x=IMS.drift.time, y=GC.runtime, fill=IMS.signal)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    viridis::scale_fill_viridis(trans = ims_transformation) +
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0)) +
    ggplot2::labs(x="IMS drift time (s)",
                  y="GC retention time (s)",
                  fill = "IMS signal")

  if (gcims %>% tibble::has_name('samplename')) plt <- plt + ggplot2::facet_wrap(~samplename, nrow = nrow, ncol = ncol)

  return(plt)
}

#' Plot features over a reference matrix
#'
#' Takes a vector of features (with the standard <GC>:<IMS> feature names) and
#' plots the values over a reference gcims matrix. Can be used to show feature
#' importance/p-values.
#'
#' @param features vector of feature values (importance)
#' @param reference_mat gcims matrix over which to plot the features
#' @param n_features (optional) plot only the top n_features
#' @param reverse_values (optional, logical) Consider small feature values as
#'   'big' for the purpose of the colour scale and picking the top n_features
#' @param ... passed to plot_gcims
#' @return gcims ggplot overlayed with feature values at the correct locations
#' @export
plot_features <- function(importance, reference_gcims, n_features = 0, reverse_values = FALSE, discretise_features = FALSE, ...) {
  stopifnot(is.matrix(reference_gcims))

  if (n_features > 0) {
    stopifnot(n_features <= nrow(importance))

    if (!reverse_values) {
      importance <- importance %>% dplyr::arrange(desc(importance))
    } else {
      importance <- importance %>% dplyr::arrange(importance)
    }

    importance <- importance[1:n_features,]
  }

  if (!discretise_features) {
    plt <- plot_gcims(reference_gcims, ...) +
      ggplot2::geom_point(data = importance,
                          mapping = ggplot2::aes(y = GC, x = IMS, colour = importance),
                          inherit.aes = FALSE,
                          shape = '.') +
      ggplot2::scale_colour_viridis_c(option = "A",
                                      direction = ifelse(reverse_values, -1, 1),
                                      begin = 0.3)
  } else {
    plt <- plot_gcims(reference_gcims, ...) +
      ggplot2::geom_point(data = importance,
                          mapping = ggplot2::aes(y = GC, x = IMS),
                          colour = 'red',
                          inherit.aes = FALSE,
                          shape = '.')
  }
  return(plt)
}
