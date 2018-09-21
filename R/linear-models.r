
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.matrix 
#' @export 
linear_model <- function(formula, data) {
  a<- all.vars(formula)
  #create response varible y
  y <- data[,a[1]]
  #create design matrix containing explanatory varibls and intercept
  mm <- model.matrix(formula, data)
  x<-mm
  output <- list()
  output$coefficients <- qr.coef(qr(x),y)
  #create lm object
  class (output) = "lm"
  return(output)
}




