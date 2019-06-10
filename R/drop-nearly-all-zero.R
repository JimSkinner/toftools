#' Drop columns in a matrix which are nearly all zeroes
#'
#' Given a matrix, drops all columns which contain more than 'threshold'
#' proportion of zeroes.
#'
#' @param mat matrix of data
#' @param max_zero_proportion value in [0,1]. Colums with more than this
#'   proportion of zeroes are dropped
#' @return the original matrix with nearly-zero columns dropped
drop_nearly_all_zero <- function(mat, max_zero_proportion = 0.5) {
  stopifnot(is.matrix(mat))
  proportion_zeroes <- apply(mat, 2, function(col) mean(col == 0))
  drop <- proportion_zeroes > max_zero_proportion

  stopifnot(ncol(mat) == length(drop))

  mat[,!drop]
}
