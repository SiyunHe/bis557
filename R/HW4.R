#Impliment Sparse matrix multiplication
sparse_add <- function(a, b) {
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")]
}
a <- data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1))
b <- data.frame(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3))
sparse_add(a, b)

#' create a new class of sparse matrix
#' @description create sparse matrix using sparse.matrix, operations defined including add, multiply and transpose
#' @param i row index of non-zero element
#' @param j col index of non-zero element
#' @param x value of non-zero element corresponding to i, j
#' @param dims dimensions of the sparse matrix
#' @return A sparse.matrix object
#' @export
sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))){
  structure(list(data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1)), dims), class = "sparse.matrix")
}

sparse_add <- function(a, b){
  if (!identical(a[[2]], b[[2]]))
    stop("dimensions not match")
  c <- merge(a[[1]], b[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c[, c("i", "j", "x")]
  sparse.matrix(c$i, c$j, c$x, dims = a[[2]])
}

sparse_multiply <- function(a, b){
  if ((a[[2]][2] != b[[2]][1]))
    stop("dimensions not match")
  colnames(b[[1]]) <- c("i2", "j2", "x2")
  c <- merge(a[[1]], b[[1]], by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  c$x <- c$x * c$x2
  c$key <- paste(c$i, c$j, sep = "-")
  x <- tapply(c$x, c$key, sum)
  key <- strsplit(names(x), "-")
  d <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  sparse.matrix(c$i, c$j, c$x, dims = c(a[[2]][1], b[[2]][2]))
}

sparse_transpose <- function(a){
  temp <- a[[1]]$i
  a[[1]]$i <- a[[1]]$j
  a[[1]]$j <- temp
  a[[2]] <- rev(a[[2]])
  return(a)
}

`+.sparse.matrix` <- function(x, y) {
  sparse_add(x, y)
}

`%*%.default` <- .Primitive("%*%")
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}
`%*%.sparse.matrix` <- function(x, y) {
  sparse_multiply(x, y)
}

`t.sparse.matrix` <- function(x) {
  sparse_transpose(x)
}