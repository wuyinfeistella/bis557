library(testthat)

context("Test the gradient descent function.")

test_that("You gradient_descent() function works in an easy case.", {
  
  data(iris)
  
  fit_gradient_descent <- gradient_descent_1(Sepal.Length ~ ., iris)
  
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  
  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-5)
})

test_that("You gradient_descent() function works with contrasts.", {
  
  data(iris)
  
  fit_gradient_descent <- gradient_descent_1(Sepal.Length ~ ., iris, 
                                           contrasts = list(Species = "contr.sum"))
  
  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
  
  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 1e-5)
})