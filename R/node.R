source("R/purity.R")
source("R/models.R")

Node <- function (id, X, Y, label, model = 0, fit = 0, r = 0, percent= 0,  cutoff= 0, l.id = 0, r.id = 0, candidates = 0, gain = 0) {
    data <- list(
                 id         = id,
                 X          = X,
                 Y          = Y,
                 label      = label,
                 candidates = candidates,
                 r          = r,
                 gain       = gain,
                 l.id       = l.id,
                 r.id       = r.id,
                 model      = model,
                 fit        = fit,
                 percent    = percent,
                 cutoff     = cutoff
                )
    class(data) <- append(class(data), "Node")
    return(data)
}

swap <- function (i, r) {
    if (i %in% r) {
        if (length(r) == 1) {
            r
        } else {
            setdiff(r, i)
        }
    } else {
        tmp <- unlist(lapply(r, as.character))
        c(tmp, as.character(i))
    }
}

load.it <- function (model, fit, Rcandidates, g, r, ...) {
    list(model = model, fit = fit, candidates = Rcandidates, gain = g, r = r)
}

load.set <- function (Y, X, model, r, purity, ...) {
    fit         <- model$call(Y, X, r)
    Rcandidates <- model$predictor(fit, X)
    i           <- gain(purity, Y, Rcandidates)

    load.it(model, fit, Rcandidates, i, r)
}

load.swap <- function (c, Y, X, model, r, purity) {
    r <- swap(c,r)
    load.set(Y, X, model, r, purity)
}

init.split <- function (c, ...) {
    mid <- floor(length(c)/2)
    sample(c, mid)
}

get.max <- function (i.delta, ...) {
    gains <- unlist(lapply(i.delta, function(x) x$gain))
    ord   <- order(gains, decreasing = TRUE)
    i.delta[ord][[1]]
}

exchange <- function (Y, X, model, fit, r, purity, ...) {
    c <- unique(Y)

    if (length(setdiff(c,r)) == 0 ) {
        return(load.it(model, fit, Y[Y %in% r], 0, r))
    } else if (length(setdiff(c,r)) == 1) {
        a <- setdiff(c,r)
        c <- as.factor(setdiff(c,a))
    }

    i.delta  <- lapply(c, function(x) load.swap(droplevels(x), Y, X, model, r, purity))
    get.max(i.delta)
}

MSplit <- function (Y, X, model, purity, ...) {
    c <- unlist(lapply(unique(Y), as.character))

    i.r         <- init.split(c)
    i.candidate <- load.set(Y, X, model, i.r, purity)
    i.gain      <- i.candidate[["gain"]]
    i.fit       <- i.candidate[["fit"]]

    max.swap    <- exchange(Y, X, model, i.fit, i.r, purity)
    iter.gain   <- max.swap$gain

    while(iter.gain - i.gain > 0) {

        i.candidate <- max.swap
        i.gain      <- i.candidate[["gain"]]
        i.fit       <- i.candidate[["fit"]]
        i.r         <- i.candidate[["r"]]

        max.swap    <- exchange(Y, X, model, i.fit, i.r, purity)
        iter.gain   <- max.swap$gain
    }
    i.candidate
}
