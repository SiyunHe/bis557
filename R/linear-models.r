
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

load("/Users/siyunhe/Desktop/Year 2/BIS557/bis557/data/lm_patho.rda")

linear_model <- function(formula, data) {
  #browser()
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
#fit_test <- linear_model(y~.,data=lm_patho)
#fit <- lm(lm_patho$y~.,lm_patho)

#install.packages("testthat")

#library(testthat)

#library(devtools)
#setwd("/Users/siyunhe/Desktop/Year 2/BIS557/bis557")
#test( )






