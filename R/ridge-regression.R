#' Fit a ridge regression model
#'
#' @description This function fits the ridge regression model.
#' @param formula a formula
#' @param lambda a number
#' @param data a data.frame
#' @return An list of rigde regression information
#' @examples
#' ridge_fit <- ridge_reg(Sepal.Length ~. , lambda = 1.2, iris)
#' @export


ridge_reg <- function(form, lambda, d) {
  rownames(d) <- NULL
  m <- model.matrix(form, d)
  y <- matrix(d[, as.character(form)[2]], ncol = 1)
  y <- y[as.numeric(rownames(m)),, drop = FALSE]
  
  ## via svd
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  ret <- list(coefficients = beta, lambda = lambda, form = form)
  class(ret) <- "ridge_reg"
  return(ret)
}

