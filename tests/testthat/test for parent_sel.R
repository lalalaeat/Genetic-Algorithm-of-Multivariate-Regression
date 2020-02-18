context("Test for parent_sel")
test_that("Scores is an appropriate vector", {
  expect_true(is.numeric(scores))
  expect_false(is.infinite(scores))
})