#' @title Gradient descent function for OLS regression
#' @description Gradient descent is a optimization algorithm which is often used to find the minimum of a function
#' @param formula Formula of the regression model 
#' @param data The data used for gradient descent
#' @param contrasts A list of contrasts for factor variables.
#' @param tolerance The threshold of iterations
#' @param alpha Learning rate
#' @return The estimated coefficients for OLS regression.
#' @examples
#' data(iris)
#' gradient_descent(Sepal.Length ~., iris, alpha=0.01, contrasts = NULL, tolerance=1e-10)
#' @export


gradient_descent_1 <- function(formula, data, contrasts = NULL, alpha=0.0001, tolerance=1e-20){
  
  d_no_na <- model.frame(formula, data)
  y_name <- as.character(formula)[2]
  Y <- matrix(d_no_na[, y_name], ncol = 1)
  X <- model.matrix(formula,d_no_na, contrasts.arg = contrasts)
  QR=qr(X)
  
  if (QR$rank==dim(X)[2]) {
    beta <- as.matrix(rep(1,ncol(X)))
    ssr <- sum((X %*% beta- Y)^2)
    
    ssr2=1

    while(abs(ssr2-ssr) > tolerance){
      ssr <- sum((X %*% beta- Y)^2)
      beta <- beta-alpha*2*(t(X)%*% X %*% beta - t(X)%*%Y)
      ssr2 <- sum((X %*% beta - Y)^2)
    }
  
    return(list(coefficients = beta))
  }
  
}














