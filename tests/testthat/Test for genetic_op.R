context("Test for genetic_op")
test_that("population is an appropriate matrix", {
  expect_true(is.logical(populaition))
  expect_true(nrow(populaition)>1)
  expect_true(ncol(populaition)>1)
})
test_that("couple_indices is an appropriate matrix", {
  expect_true(is.logical(populaition))
  expect_equal(nrow(couple_indices), 2)
  expect_true(ncol(populaition)>1)
})