library(nnet)
library(MASS)

NET <- function (Y, X, r, ...) {
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    nnet(Y.hat ~ ., data = X, size = 2, trace = FALSE)
}

GLM <- function (Y, X, r, ...) {
    r <- c("setosa")
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    glm(Y.hat ~ ., data = X, family = gaussian)
}

LDA <- function (Y, X, r, ...) {
    r <- c("setosa")
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    lda(Y.hat ~ ., data = X)
}

LM <- function (Y, X, r, ...) {
    r <- c("setosa")
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    lm(Y.hat ~ ., data = X)
}


