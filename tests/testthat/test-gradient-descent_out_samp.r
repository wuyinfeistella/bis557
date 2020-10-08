library(testthat)

context("Test the gradient descent function.")

test_that("You gradient_descent() function works in an easy case.", {
  
  data(iris)
  
  fit_gradient_descent <- gradient_descent(Sepal.Length ~ Petal.Width, iris)
  
  fit_lm <- lm(Sepal.Length  ~ Petal.Width, iris)
  
  expect_equivalent(fit_lm$coefficients, fit_gradient_descent$coefficients,
                    tolerance = 0.1)
})



