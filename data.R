library(RCurl)

get.iris <- function () {
    X <- iris[,1:4]; Y <- iris[,5]
    list(X = X, Y = Y)
}

get.train <- function (flds, k, x, ...) {
    as.vector(unlist(flds[setdiff(1:k,x)]))
}

kfolds.accuracy <- function (d, flds, k, x, ...) {
    X.train <- d[["X"]][get.train(flds,k,x), ]
    Y.train <- d[["Y"]][get.train(flds,k,x)]
    X.test  <- d[["X"]][flds[[x]], ]
    Y.test  <- d[["Y"]][flds[[x]]]
    dt      <- tree(X.train, Y.train, Pure = Info, model = SVM)

    table(pred = m.predict(dt, X.test), actu = Y.test)
}

get.letters <- function () {
    letter.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data"
    letter.url  <- getURL(letter.file)
    letter.data <- read.csv(textConnection(letter.url), header = FALSE)
    letter.data <- droplevels(subset(letter.data, V1 %in% c("A","B", "C","D")))
    list(X = letter.data[,-1], Y = letter.data[, 1])
}



