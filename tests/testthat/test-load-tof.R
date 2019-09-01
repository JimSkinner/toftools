context("load_tof")

test_that("load_tof functions as expected", {
  pigs <- load_tof("../../data-raw/pigs.txt")
  expect_equal(dim(pigs), c(281, 161))
})

test_that("extract_labels gives expected output on examples", {
  names <- c("a", "a", "a", "b", "b")
  labels <- extract_labels(names)
  expect_equal(labels, names)

  # Prepending letters to everything should add these letters to the class names
  names2 <- paste0("123", names)
  labels2 <- extract_labels(names2)
  expect_equal(labels2, names2)

  # Appending extra letters to everything should not change the class names
  names3 <- paste0(names2, "123")
  labels3 <- extract_labels(names3)
  expect_equal(labels3, labels2)

  # Appending an extra etter to 1 sample should not change the labels
  names4 <- names3
  names4[1] <- paste0(names4[1], "test")
  labels4 <- extract_labels(names4)
  expect_equal(labels4, labels3)

  # Case should have no impact
  names5 <- names4
  names5[c(1, 3, 5)] <- toupper(names5[c(1, 3, 5)])
  labels5 <- extract_labels(names5)
  expect_equal(labels5, labels4)

  # Fail cases
  expect_error(extract_labels(""))                 # Not enough names
  expect_error(extract_labels(c("a")))             # Not enough names
  expect_error(extract_labels(c("a", "a")))        # Not enough classes
  expect_error(extract_labels(c("a", "b", "c")))   # Too many classes
})