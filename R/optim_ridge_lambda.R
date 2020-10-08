#' @title optimize a lambda for a ridge regression model by cross validation
#' @description A function that chooses the best lambda for a ridge regression
#' @param X The predictors of the ridge regression model 
#' @param Y The response variable of the ridge regression model 
#' @param lambda possible penalizing coefficients
#' 
#' @return The optimized lambda
#' @examples
#'  X = matrix(c(rep(1, 10), seq(2,20,2)+rnorm(10)), ncol=2)
#'  y = c(seq(10,100,10)+rnorm(10))
#'  optim_lamb(X,y, lambdas=c(1,2,3,4,5), 5)
#' @export
 
optim_lamb <- function(X, y, lambdas, folds){
   b<-NULL
   #assign folds to the data
   fold <- sample(rep(1:folds, len = length(y)))
   
   #create test and train data
   for (i in 1:folds){
     trainX = X[which(fold !=i ),]
     trainy = y[which(fold !=i )]
     testX = X[which(fold ==i ),]
     testy = y[which(fold ==i )]
   }
   
   
error<- NULL
b<-NULL
sse<-NULL
final_lamb = NULL
  
 for (i in 1:length(lambdas)){
     b <- cbind(b, ridge(trainX, trainy, lambdas[i])$coefficients)
    }

pred_y <- testX %*% b
sse <- apply( (pred_y-testy)^2 ,2, mean)
final_lamb <- lambdas[which.min(sse)]

print(final_lamb)

 }
 




