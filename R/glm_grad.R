#' @title A first-order solution for the GLM maximum likelihood problem
#' @description a first-order solution for the GLM maximum likelihood problem using only gradient information without the Hessian matrix
#' @param X The predictors of the glm
#' @param y The response variable of the glm
#' @param family The link function
#' @param niter The number of iterations
#' @param lambda The learning rate
#' @param update Either a constant step size or an adaptive step size
#' @param gamma The momentum step size
#' @return The estimated coefficients for the glm
#' @examples
#' #reference CASL 5.3
#' n = 1000; p = 5
#' X <- cbind(1, matrix(rnorm(n*(p-1)), ncol=p-1))
#' y = sample(c(0,1), replace=TRUE, size=n)
#' glm(X, Y, update=FALSE)
#' @export


glm_grad <- function(X, y, family=poisson(link="log"), niter=1000, lambda=1e-05, update=FALSE, gamma=0.95){
  
 b <- rep(0, ncol(X))
 
 if(update==FALSE){
   for (i in 1:niter){
    
    b_old <- b
    eta <- X %*% b
    mu <- family$linkinv(eta)
    gradient <- t(X) %*% (Y-mu)
    
    b = b+lambda*gradient
   }
}
    
  if(update==TRUE){
    for (i in 1:niter){
      
      b_old <- b
      eta <- X %*% b
      mu <- family$linkinv(eta)
      gradient <- t(X) %*% (Y-mu)
      gradient2 <- gamma*b +(1-gamma)*gradient
      b = b+lambda*gradient2
    }
  }
  return(list(b))
}
  






