#' @title A classification model with multiple classes
#' @description A classification model generalizing logistic regression with multiple classes
#' @param X The predictors of the classification model 
#' @param Y The response variable of the classification model 
#' @param niter The number of iterations 
#' @return The estimated probabilities and the misclassification rate
#' @examples
#' X <- matrix(rnorm(500), ncol=5)
#' y <- sample(c(1,2,3) , size=100, replace = T)
#' class_log(X,y, class=3)
#' @export


class_log <- function(X, y, class, niter=100){
  b = b_old = matrix(0, nrow = class, ncol= ncol(X))
  
  for(i in 1:niter){
    for (j in 1:class) {
      b_old[j,] <- b[j,]
      p <- 1/(1+exp(-X %*% b[j,]))
      D <- diag(as.vector(p))
      XtDX <- t(X) %*% D %*% X
      score <- t(X) %*% (1*(y==unique(y)[j]) - p)
      d <- solve(XtDX, score)
      b[j,] <- b[j,] + d
      
    }
  }
  
  #calculate probabilities
  p = matrix(0,nrow = nrow(X), ncol = class)
  total = exp(X %*% b[1,])+ exp(X %*% b[2,])+ exp(X %*% b[3,])
  p[, 1] = exp(X %*% b[1,])/total
  p[, 2] = exp(X %*% b[2,])/total
  p[, 3] = exp(X %*% b[3,])/total

  #assign classification
  fit <- unique(y)[apply(p, 1, which.max)]
  misclass <- mean(y != fit)
  
  return(list(fit=p, misclassification = misclass))
}
    



  
  
