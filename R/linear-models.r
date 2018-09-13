
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats svd
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  var = all.vars(formula)
  y = data[, var[1]]
  if(formula == 'y~.'){
    X = data[, -which(colnames(data) == var[1])]
    X = cbind(rep(1, nrow(X)), X)
    colnames(X)[1] = 'Intercept'
  }else {
    X = data[, var[-1]]
    X = cbind(rep(1, nrow(X)), X)
    colnames(X)[1] = 'Intercept'
  }
  
  X_svd = svd(X)
  U = X_svd$u
  V = X_svd$v
  D_inverse = diag(1 / X_svd$d)
  beta = V %*% D_inverse %*% t(U) %*% y
  my_coef = list(coefficients = beta)
  class(my_coef) = 'lm'
  return(my_coef)
}
