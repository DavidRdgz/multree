library(nnet)
library(MASS)
library(e1071)

NET <- function (Y, X, r, ...) {
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    nnet(Y.hat ~ ., data = X, size = 2, trace = FALSE)
}

GLM <- function (Y, X, r, ...) {
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    glm(Y.hat ~ ., data = X, family = gaussian)
}

SVM <- function (Y, X, r, ...) {
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    Y.hat <- as.factor(Y.hat)
    svm(Y.hat ~ ., data = X)
}

LDA <- function (Y, X, r, ...) {
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    lda(Y.hat ~ ., data = X)
}

LM <- function (Y, X, r, ...) {
    Y.hat <- unlist(lapply(Y, function(x) x %in% r))
    lm(Y.hat ~ ., data = X)
}

