library(testthat)
library(MASS)

context("Test the lambdas optimization function.")

test_that("You optim_lamb() function works in an easy case.", {
  
  #  let's test on a 500x6 matrix
  b <- c(0,1, rep(0, 4))
  X <- matrix(rnorm(3000), ncol = 6)
  X[,2] <- X[,2]*0.001 + X[,6]*0.999
  y <- X %*% b + rnorm(500)
  
  data <- as.data.frame(cbind(y,X))
  colnames(data)[1]="response"
  lambdas= seq(3,5, len = 10)
  folds=5
  
  
  fit_lmridge= lm.ridge(response ~ ., data, lambda = lambdas)
  fit_ridge = optim_lamb(X, data$response, lambda = lambdas, folds=folds)
  
  
  lambda_MASS = lambdas[which.min(fit_lmridge$GCV)]
  lambda_bis557 = optim_lamb(X,y,lambdas,folds)
  
  
  expect_equivalent(lambda_MASS, lambda_bis557,
                    tolerance = 0.5)
  
})




