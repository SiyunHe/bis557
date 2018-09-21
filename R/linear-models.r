
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  a<- all.vars(formula)
  y <- data[,a[1]]
  mm <- model.matrix(formula, data)
  x<-mm
  output <- list()
  output$coefficients <- qr.coef(qr(x),y)
  #output$call <- call("linear_model",formula)
  class (output) = "lm"
  return(output)
}
