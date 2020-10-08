#' @title Fit linear models by the linear_model() function
#' @description This linear_model function aims to fit OLS linear regressions
#' @param formula The formula of the regression model
#' @param data A data frame that's being used in this model
#' @param contrasts A list of constasts for factor variables.
#' @return A list containing formula, estimated coefficients and contrasts list.
#' @examples
#' data(iris)
#' fit_linear_model <- linear_model(Sepal.Length ~., iris, contrasts = list(Species = "contr.sum"))
#' @export

 linear_model <- function(formula, data, contrasts = NULL) {

   d_no_na <- model.frame(formula, data)
   X <- model.matrix(formula, data, contrasts=contrasts)
   Y <- d_no_na[,1]
   beta <- solve.qr(qr(X), Y)
   beta[which(beta == 0)] <- NA

   fitted = X %*% beta
   residuals = Y - X %*% beta

   fitted.model = list(formula = formula, coefficients = beta, contrasts = contrasts)

   return(fitted.model)
 }






