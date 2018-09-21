
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

#load("/Users/siyunhe/Desktop/Year 2/BIS557/bis557/data/lm_patho.rda")

linear_model <- function(formula, data) {
  #browser()
  lm(formula, data)
}
#fit_test <- linear_model(y~.,data=lm_patho)
#fit <- lm(lm_patho$y~.,lm_patho)
#fit_test <- linear_model(Sepal.Length ~., iris)
#fit <- lm(Sepal.Length ~., iris)


#install.packages("testthat")

#library(testthat)

#library(devtools)
#setwd("/Users/siyunhe/Desktop/Year 2/BIS557/bis557")
#test( )






