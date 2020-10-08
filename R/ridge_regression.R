#' @title A ridge regression function
#' @description A ridge regression that takes into account colinear (or nearly colinear) regression variables
#' @param X The predictors of the ridge regression model 
#' @param Y The response variable of the ridge regression model 
#' @param lambda The penalizing parameter 
#' @return The estimated coefficients for ridge regression and the penalizing parameter lambda
#' @examples
#' X = matrix(c(rep(1, 10), seq(2,20,2)+rnorm(10)), ncol=2)
#' y = c(seq(10,100,10)+rnorm(10))
#' ridge(X, y, lambda=0.01)
#' @export

ridge <- function(X,Y,lambda) {
  svd_x <- svd(X)
  
  Sigma <- diag(svd_x$d)
  
  lambda_I <- diag(rep(lambda, length(svd_x$d)))
  
  beta <- svd_x$v %*% solve(Sigma^2+lambda_I) %*% Sigma %*% t(svd_x$u) %*% Y
  
  fitted.model = list(coefficients = beta, lambda = lambda_I)
  
  return(fitted.model)
}








