ridge_train <- read.csv("ridge_train.csv")
dir.create("../data")
save(ridge_train, file = "../data/ridge_train.rda")

ridge_test <- read.csv("ridge_test.csv")
dir.create("../data")
save(ridge_test, file = "../data/ridge_test.rda")