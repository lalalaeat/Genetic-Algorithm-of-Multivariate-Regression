context("Test for mutated")#only test population in this case since mut_prob has been tested in the test for select function
test_that("population is an appropriate matrix", {
  expect_true(is.logical(population))
})
