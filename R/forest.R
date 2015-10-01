source("R/tree.R")

MulForest <- function (args = 0) {
    data <- list(
                 forest = list(),
                 call   = args
                 )
    class(data) <- append(class(data), "MulForest")
    return(data)
}

m.top <- function(m, ...) {
    stats <- lapply(apply(m,1, list), table)
    top   <- lapply(stats, function(x) names(sort(x, decreasing = TRUE))[1])
    unlist(top)
}


#' Predicting classes from a multivariate decision forest
#'
#' This function predict classes from a dataframe 'x' based on a 'mulforest' object.
#'
#' @param f is a tree from fitted by 'mulforest'
#' @param X is a dataframe with the same columns fitting 'f'.
#'
#' @return a vector of class labels for each row from 'x'
#'
#' @author David Rodriguez
#' @details
#' This function provides class predictions from a dataframe 'x' based on a 'mulforest' object. Nested in this prediction function are the associated predict functions for a 'glm', 'svm', 'nnet', 'lm', 'lda' object.
#'
#' @seealso \code{glm, svm, nnet, lm, lda}
#' @examples
#' Y  <- iris[,5]
#' X  <- iris[,1:4]
#' f  <- mulforest(Y, X)
#' p  <- mf.predict(f, X)
#' table(pred = p, actu = Y)

mf.predict <- function(f, X) {
    p   <- lapply(f$forest, function(x) m.predict(x, X))
    d   <- do.call(cbind, p)
    m.top(d)
}

add.tree <- function (f, t, ...) UseMethod("add.tree")
add.tree.MulForest <- function (f, t, ...) {
    f$forest[[length(f$forest)+1]] <- t
    f
}

#' Building a multivariate decision forest
#'
#' This function develops a multivariate decision forest classifier in 'x' to the class of 'y'.
#'
#' @param X a dataframe of continuous or categorical values.
#' @param Y a vector of classes set as factors.
#' @param purity may be: \code{gini, information, twoing}. Sets impurity measure for a node.
#' @param split may be: \code{glm, svm, net, lm, lda}. Sets the hyper surface spliter at nodes in the tree.
#' @param a may be: any real value. Cuts-off tree growth if subset has purity greater than \code{a}.
#' @param tune may be a list of arguments used instead of presets for \code{glm, svm, net, lm, lda}.
#' @param feature.space List identifying the columns of the matrix, and features to extract using \code{window, w, k, features} variables.
#' @param window may be: \code{all,dots,bars,kbars}. This provides at each split, either selecting all the columns (all) or randomly selecting #(cols/2) (dots), or selecting a random sequence of columns (bars).
#' @param w may be: used with \code{kbars} specifies window size.
#' @param k may be: used with \code{kbars} specifies the slide (number of \code{w} windows).
#' @param features may be: \code{i, s, m, a}. These represent: i = identity, s = sum, m = mean, a = aboslute mean aggregates over filter columns.
#' @param size may be: \code{int}. Determines the number of multivariate trees to fit.
#'
#' @return an MulForest object with a forest consisting of trees (multree objects) and their attributes.
#' @author David Rodriguez
#' @details
#' This function develops a multivariate decision forest classifier to predict an unseen 'x's class 'y'. That is, this replicates the random forest algorithm but with multivariate splitting functions. The options of using a generalized linear model, support vector machine, neural network, or linear model to construct a hyper-surface splitting plane in ways not found in random forest (by Breiman).
#'
#' @seealso \code{glm, svm, nnet, lm, lda}
#' @examples
#' Y  <- iris[,5]
#' X  <- iris[,1:4]
#' dt <- mulforest(Y, X, "net")


mulforest <- function(Y, X, split = "svm", a = .8,  tune = NULL, purity = "info", feature.space = list(window = "dots", w = 0, k = 0, features = "i"), size = 10, ...) {
    args <- mget(names(formals()),sys.frame(sys.nframe()))[-c(1,2)]

    f    <- MulForest(args)
    pb   <- txtProgressBar(min = 0, max = size, style = 3)

    #randomize net size, svm degree, glm to glmnet or lasso
    for (i in seq(size)) {
        setTxtProgressBar(pb, i)
        ss  <- sample(seq(nrow(X)), round(4/5*nrow(X),0), replace = FALSE)
        X.x <- droplevels(X[ss,])
        Y.y <- droplevels(Y[ss])
        t   <- multree(Y.y,X.x, split, a,  tune, purity, feature.space)
        f   <- add.tree(f, t)
    }
    f
}



