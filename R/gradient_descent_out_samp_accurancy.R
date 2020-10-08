#' @title Gradient descent function for OLS regression with the penalty based on the out-of-sample accuracy
#' @description Gradient descent is a optimization algorithm which is often used to find the minimum of a function
#' @param formula Formula of the regression model 
#' @param data The data used for gradient descent
#' @param contrasts A list of contrasts for factor variables.
#' @param tolerance The threshold of iterations
#' @param alpha Learning rate
#' @return The estimated coefficients for OLS regression.
#' @examples
#' data(iris)
#' gradient_descent(Sepal.Length ~Petal.Width, iris, alpha=0.01, contrasts = NULL, tolerance=1e-10)
#' @export


gradient_descent <- function(formula, data, contrasts = NULL, alpha=0.0001, tolerance=1e-20){
  
  d_no_na <- model.frame(formula, data)
  y_name <- as.character(formula)[2]
  Y <- matrix(d_no_na[, y_name], ncol = 1)
  X <- model.matrix(formula,d_no_na, contrasts.arg = contrasts)
  QR=qr(X)
  
  sample <- sample(1:nrow(X), size = floor(0.8 * nrow(X)))
  trainX <- X[sample, ]
  testX <- X[-sample, ]
  trainY <- Y[sample]
  testY <- Y[-sample]
  
  
  if (QR$rank==dim(trainX)[2]) {
    beta <- as.matrix(rep(1,ncol(trainX)))
    ssr <- sum((testX %*% beta- testY)^2)
    ssr2 <- 1
    
    while(abs(ssr2-ssr) > tolerance){
      ssr <- sum((testX %*% beta- testY)^2)
      beta <- beta-alpha*2*(t(trainX)%*% trainX %*% beta - t(trainX)%*%trainY)
      ssr2 <- sum((testX %*% beta - testY)^2)
      
    }
    
    return(list(coefficients = beta))
  }
}
