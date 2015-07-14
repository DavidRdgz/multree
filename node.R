source("purity.R")
source("models.R")

Node <- function (id, X, Y, label, model = 0, r = 0, percent= 0,  cutoff= 0, l.id = 0, r.id = 0, candidates = 0, gain = 0) {
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
                 percent    = percent,
                 cutoff     = cutoff
                )
    class(data) <- append(class(data), "Node")
    return(data)
}

get.Gain <- function (Pure, Y, Rcandidates, ...) {
    IR     <- Pure(Y[Rcandidates])
    IL     <- Pure(Y[!Rcandidates])
    p      <- sum(Rcandidates)/length(Y)

    if (identical(Pure, match.fun("Twoing"))) {
        return(AbsSum(IL, IR, p))
    } else {
        return(Gain(Pure(Y), IL, IR, p))
    }
}

get.GainRatio <- function (gain, Rcandidates) {
    p <- sum(Rcandidates)/length(Rcandidates)
    gain/Potential(p)
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

predRCandidates <- function (n, X, model) {
    if (identical(model, LDA)) {
        preds <- c(predict(n, X)[[3]])
    } else {
        preds <- c(predict(n, X))
    }
    Rcandidates <- preds > 0
}

load.it <- function (n, Rcandidates, g, r, ...) {
    list(model = n, candidates = Rcandidates, gain = g, r = r)
}

load.set <- function (Y, X, model, r, Pure = Info, ...) {
    n           <- model(Y, X, r)
    Rcandidates <- predRCandidates(n, X, model)
    i           <- get.Gain(Pure, Y, Rcandidates)

    load.it(n, Rcandidates, i, r)
}

load.swap <- function (c, Y, X, model, r) {
    r <- swap(c,r)
    load.set(Y, X, model, r)
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

maxi <- function (Y, X, model, r, ...) {
    c        <- unique(Y)
    i.delta  <- lapply(c, function(x) load.swap(droplevels(x), Y, X, model, r))

    get.max(i.delta)
}

MSplit <- function (Y, X, model, Pure, ...) {
    c <- unlist(lapply(unique(Y), as.character))
    r <- init.split(c)

    i.candidate <- load.set(Y, X, model, r)
    i.gain      <- i.candidate[["gain"]]

    max.swap    <- maxi(Y, X, model, r)
    iter.gain   <- max.swap$gain

    while(iter.gain - i.gain > 0) {
        
        i.candidate <- max.swap
        i.gain      <- i.candidate[["gain"]]
        r           <- i.candidate[["r"]]

        max.swap    <- maxi(Y, X, model, r)
        iter.gain   <- max.swap$gain
    }
    i.candidate
}

