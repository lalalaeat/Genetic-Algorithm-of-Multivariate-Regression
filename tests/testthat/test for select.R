library(testthat)
context("Input arguments test for main function")
test_that("dat is a dataframe of covarites and respond", {
  expect_true(is.data.frame(dat))
  expect_true(is.numeric(c(x,y)))
  expect_true(is.numeric(dat[,c(x,y)]))
  expect_false(is.infinite(dat[,c(x,y)]))
  expect_true(length(x)>0)
  expect_true(length(y)>0)
})
test_that("glm_family is a family object", {
  expect_true(is.object(glm_family))
  
})
test_that("P is an appropriate number", {
  expect_true(is.integer(P))
  expect_equal(P>0)
})
test_that("fit_func is a function", {
  expect_true(is.function(fit_func) || fit_func == "aic")
})
test_that("genetic_op is a function", {
  expect_true(is.function(genetic_op))
})
test_that("num_iter is an appropriate number", {
  expect_true(is.integer(num_iter))
  expect_true(num_iter>1)
})
test_that("parent_sel is a function", {
  expect_true(is.function(parent_sel))
})
test_that("mut_prob is an appropriate probability of mutation", {
  expect_true(is.numeric(mut_prob))
  expect_true(mut_prob > 0 && mut_prob < 1)
})
test_that("elitism is an appropriate bool variable", {
  expect_true(is.logical(elitism))
})
