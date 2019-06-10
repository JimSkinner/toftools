context("drop nearly all zero")

test_that("drop_nearly_all_zero works as expected", {
  upper_tri_mat <- upper.tri(matrix(1, nrow = 10, ncol = 11)) + 0

  expect_lt(
    ncol(drop_nearly_all_zero(upper_tri_mat, 0.2)),
    ncol(drop_nearly_all_zero(upper_tri_mat, 0.8))
  )

  expect_lt(
    ncol(drop_nearly_all_zero(as.matrix(pigs[,-1]), 0.5)),
    ncol(as.matrix(pigs[,-1]))
  )

  threshold <- 0.4
  near_zeroes_dropped <- drop_nearly_all_zero(upper_tri_mat, threshold)
  for (i in 1:ncol(near_zeroes_dropped)) {
    expect_lte(mean(near_zeroes_dropped[,i] == 0), threshold)
  }
})
