source("tree.R")

Forest <- function () {
    data <- list(
                 forest = list()
                 )
    class(data) <- append(class(data), "Tree")
    return(data)
}

add.Tree <- function (f, t, ...) {
    f$forest[[length(f$forest) + 1]] <- t
    f
}

top.pred <- function (tmp.pred, ...) {
    names(sort(table(tmp.pred)))[[1]]
}

rf.predict <- function (f, X, ...) {
    pred <- c()
    for (row in 1:nrow(X)){
        tmp.pred <- c()
        for (t in f$forest) {
            tmp.pred <- c(tmp.pred, predict(t, X[row,]))
        }
    pred <- c(pred, top.pred(tmp.pred))
    }
    pred
}

forest <- function (X, Y, n, grow = TRUE, splitter = Branch, ...) {
    f <- Forest()

    for (i in 1:n) {
        print(i)
        t <- tree(X,Y, thresh = KTile, is.forest = grow, splitter = splitter)
        f <- add.Tree(f, t)
    }
    f
}
