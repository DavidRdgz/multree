source("R/node.R")
source("R/models.R")
source("R/purity.R")


MulTree <- function (args = 0) {
    data <- list(
                 tree = list(),
                 call = args
                 )
    class(data) <- append(class(data), "MulTree")
    return(data)
}

get.top.percent <- function (Y, ...) {
    percent <- sort(table(Y), decreasing = TRUE) / length(Y)
    round(percent[[1]], 2)
}

get.top <- function (Y, ...) {
    names(sort(table(Y), decreasing = TRUE))[[1]]
}

get.X <- function (XY, subset, ...) {
    droplevels(XY[[2]][subset,])
}

get.Y <- function (XY, subset, ...) {
    droplevels(XY[[3]][subset])
}

setup.queue <- function (id, X, Y, ...) {
    list(list(id = id, X = X, Y = Y, label = get.top(Y), percent = get.top.percent(Y)))
}

push.queue <- function (q, id, X, Y, ...) {
    q[[length(q) +1]] <- list(id = id,X = X,Y= Y,label= get.top(Y), percent = get.top.percent(Y))
    q
}

children.payload <- function (XY, s, id,  ...) {
    children <- list(r.id = id + 1 , l.id = id + 2)
    c(XY, s, children)
}

nochildren.payload <- function (XY, s, ...) {
    c(XY, s)
}

grow.tree <- function (t, n, ...) UseMethod("grow.tree")
grow.tree.MulTree <- function (t, n, ...) {
    t$tree[[length(t$tree)+1]] <- n
    t
}

get.branch <- function(t, n, ...) UseMethod("get.branch")
get.branch.MulTree <- function (t, n, ...) {
    for (node in t$tree) {
        if (node[["id"]] == n) {
            return(node)
        }
    }
}

get.leaves <- function (t, ...) {
    leaves <- list()
    for (node in t$tree) {
        if (node$l.id == 0 && node$r.id == 0) {
            leaves[[length(leaves) + 1 ]] <- node
        }
    }
    leaves
}

get.internal <- function (t, ...) {
    internal <- list()
    for (node in t$tree) {
        if (node$l.id != 0 && node$r.id != 0) {
            internal[[length(internal) + 1 ]] <- node
        }
    }
    internal
}

is.a.leaf <- function (n, ...) {
    n$l.id == 0
}

get.parent <- function (t, id) {
    for (node in t$tree) {
        if (node$l.id == id || node$r.id == id) {
            return(node)
        }
    }
}

#' Predicting classes from a multivariate decision tree
#'
#' This function predict classes from a dataframe 'x' based on a 'multree' object.
#'
#' @param t is a tree from fitted by 'multree'
#' @param X is a dataframe with the same columns fitting 't'.
#'
#' @return a vector of class labels for each row from 'x'
#'
#' @author David Rodriguez
#' @details
#' This function provides class predictions from a dataframe 'x' based on a 'multree' object. Nested in this prediction function are the associated predict functions for a 'glm', 'svm', 'nnet', 'lm', 'lda' object.
#'
#' @seealso \code{glm, svm, nnet, lm, lda}
#' @examples
#' Y  <- iris[,5]
#' X  <- iris[,1:4]
#' dt <- multree(Y, X)
#' p  <- m.predict(dt, X)
#' table(pred = p, actu = Y)

m.predict <- function(t, X, ...) {
    predictions <- c()
    cnt         <- nrow(X)
    pb          <- txtProgressBar(min = 0, max = cnt, style = 3)

    for (iter in seq_along(1:cnt)) {
        setTxtProgressBar(pb, iter)
        node <- get.branch(t, 1)

        while (node$r.id != 0 && node$l.id != 0) {
            go.right <- node$model$predictor(node$fit, X[iter, , drop = FALSE])

            if (go.right) {
                node <- get.branch(t, node$r.id)
            } else {
                node <- get.branch(t, node$l.id)
            }
        }
        predictions  <- c(predictions, node$label)
    }
    predictions
}

a.bag <- function (X, Y, n = 200, ...) {
    s <- length(Y)
    sub <- sample(1:s, n, replace = TRUE)
    list(X = X[sub,], Y = Y[sub])
}

get.children <- function (X, Y, subset, ...) {
    RX <- droplevels(X[subset,])
    RY <- droplevels(Y[subset])
    LX <- droplevels(X[!subset,])
    LY <- droplevels(Y[!subset])
    list( "Right" = list(X = RX, Y = RY), "Left" = list( X = LX, Y = LY))
}

push.children <- function (q, n.id, children, ...) {
    q <- do.call(push.queue, c(list(q = q, id = n.id+1), children[["Right"]]))
    q <- do.call(push.queue, c(list(q = q, id = n.id+2), children[["Left"]]))
    q
}

is.pure <- function (Y, a, ...) {
    if ( length(Y) < 2) {
        return(TRUE)
    }
    n <- table(Y)/length(Y) > a
    sum(n) > 0
}

no.children <- function (t, XY, s, ...) {
    payload <- nochildren.payload(XY, s)
    n       <- do.call(Node, payload)
    t       <- grow.tree(t, n)
}

yes.children <- function (t, XY, s, id, ...) {
    payload  <- children.payload(XY, s, id)
    n        <- do.call(Node, payload)
    t        <- grow.tree(t, n)
    t
}

all.true.false <- function (Y, ...) {
    n <- sum(Y)/length(Y)
    if (n == 0 || n  ==1) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

rangle <- function(vec) {
    if (length(vec) == 1) {
        return(function(x) vec)
    } else {
        return(function(x) sample(vec, 1))
    }
}

a.tune <- function(tune) {
    if (!is.null(tune)) {
        if ("size" %in% names(tune)) {
            tune[["size"]] = rangle(tune[["size"]])
        }
        if ("degree" %in% names(tune)) {
            tune[["degree"]] = rangle(tune[["degree"]])
        }
    }
    tune
}

init.args <- function(args, Y, X, ...) {

    args[["dist.y"]] <- table(Y)
    args[["m.x"]]    <- nrow(X)
    args[["tune"]]   <- a.tune(args[["tune"]])
    args[["purity"]] <- Purity(args[["purity"]])
    args
}


#' Building a multivariate decision tree
#'
#' This function develops a multivariate decision tree classifier in 'x' to the class of 'y'.
#'
#' @param X a dataframe of continuous or categorical values.
#' @param Y a vector of classes set as factors.
#' @param purity may be: \code{gini, information, twoing}. Sets impurity measure for a node.
#' @param split may be: \code{glm, svm, net, lm, lda}. Sets the hyper surface spliter at nodes in the tree.
#' @param a may be: any real value. Cuts-off tree growth if subset has purity greater than \code{a}.
#' @param tune may be a list of arguments used instead of presets for \code{glm, svm, net, lm, lda}.
#' @param feature.space List identifying the columns of the matrix, and features to extract using \code{window, w, k, features} variables.
#' @param window maybe be: \code{all,dots,bars,kbars}. This provides at each split, either selecting all the columns (all) or randomly selecting #(cols/2) (dots), or selecting a random sequence of columns (bars).
#' @param w may be: used with \code{kbars} specifies window size.
#' @param k may be: used with \code{kbars} specifies the slide (number of \code{w} windows).
#' @param features may be: \code{i, s, m, a}. These represent: i = identity, s = sum, m = mean, a = aboslute mean aggregates over filter columns.
#'
#'
#' @return an MulTree object with a tree consisting of nodes and their attributes.
#' @author David Rodriguez
#' @details
#' This function develops a multivariate decision tree classifier to predict an unseen 'x's class 'y'. The options of using a generalized linear model, support vector machine, neural network, or linear model to construct a hyper-surface splitting plane in ways not found in decision trees.
#'
#' @seealso \code{glm, svm, nnet, lm, lda}
#' @examples
#' Y  <- iris[,5]
#' X  <- iris[,1:4]
#' dt <- multree(Y, X, "svm")

multree <- function(Y, X, split = "svm", a = .8,  tune = NULL, purity = "gini", feature.space = list(window = "all", w = 0, k = 0, features = "i"), ...) {
    args <- mget(names(formals()),sys.frame(sys.nframe()))[-c(1,2)]

    args <- init.args(args, Y, X)

    id <- 1
    q  <- do.call(setup.queue, list(id, X, Y))
    t  <- MulTree(args)

    while(length(q) > 0) {
        XY <- q[[1]]; q <- q[-1]

        if (is.pure(XY[["Y"]], a)) {
            n <- do.call(Node, XY)
            t <- grow.tree(t, n)
        } else {
            args[["model"]] <- do.call(Model, c(args, XY))
            s               <- do.call(MSplit, c(XY, args))
            subset          <- s[["candidates"]]

            if (is.null(subset) || all.true.false(subset)) {
                t        <- no.children(t, XY, s)
            } else {
                children <- do.call(get.children, c(XY, list(subset = subset)))
                q        <- push.children(q, id, children)
                t        <- yes.children(t, XY, s, id)
                id <- id + 2
            }
        }
    }
    t
}
