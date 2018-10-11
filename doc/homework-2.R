## ------------------------------------------------------------------------
library(bis557)
library(foreach)
data(ridge_train)
data(ridge_test)

predict.ridge_reg <- function(object, ...) {
  newdata <- list(...)[[1]]
  m <- model.matrix(object$form, newdata)
  m %*% object$coefficients
}

lambda_seq <- seq(25, 40, by = 0.01)
mse <- foreach(lambda = lambda_seq, .combine = c) %do% {
  fit <- ridge_reg(y ~., lambda, ridge_train)
  error <- ridge_test$y - predict.ridge_reg(fit, ridge_test)
  mean(error^2)
}

plot(log(lambda_seq), mse, xlab = 'log lambda', ylab = 'out-of-sample mse', main = 'Out of Sample mean squared errors vary with lambda')


