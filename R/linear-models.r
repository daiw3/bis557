
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export

# linear_model3 <- function(formula, data){
#   fit <- lm(formula, data)
#   return(fit)
# }

linear_model <- function(formula, data) {
  var = all.vars(formula)
  y = data[, var[1]]
  X = model.matrix(formula, data)
  
  X_QR = qr(X)
  beta = solve.qr(X_QR, y)
  beta[which(beta == 0)] = NA
  
  fitted = X %*% beta
  residuals = y - fitted
  linear_model_fit = list(coefficients = beta, residals = residuals, fitted.values = fitted,
                 rank = ncol(X), weights = NULL, df.residual = nrow(X) - ncol(X), 
                 call = call('lm', formula), terms = terms(x = formula, data = data),
                 contrasts = NA, xlevels = NA, offset = NA, y = y, x = X, 
                 model = formula, na.action = NA, qr = X_QR)
  class(linear_model_fit) = 'lm'
  return(linear_model_fit)
}

linear_model2 <- function(formula, data) {
  options(digits = 4)
  var = all.vars(formula)
  y = data[, var[1]]
  X = model.matrix(formula, data)
  
  X_svd = svd(X)
  U = X_svd$u
  V = X_svd$v
  D_inverse = diag(1 / X_svd$d)
  beta = V %*% D_inverse %*% t(U) %*% y
  beta[which((beta - 0) < 1e-8)] = NA
  
  my_coef = list(coefficients = beta, call = NA, rank = NA)
  class(my_coef) = 'lm'
  return(my_coef)
}
