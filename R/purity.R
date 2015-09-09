
Purity <- function (type = "info") {

    pcall <- pre2.call(type)

    data <- list(
                 type       = type,
                 call       = pcall,
                 gain       = NULL,
                 gain.ratio = NULL
                )
    class(data) <- append(class(data), "Purity")
    return(data)
}

pre2.call <- function (type, ...) {
    purity <- function(Ys, ...) {
        do.call(toupper(type), list(Ys = Ys))
    }
}

gain <- function(Purity, Y, Rcandidates, ...) UseMethod("gain")
gain.Purity <- function (Purity, Y, Rcandidates, ...) {
    IR     <- Purity$call(Y[Rcandidates])
    IL     <- Purity$call(Y[!Rcandidates])
    p      <- sum(Rcandidates)/length(Y)

    if (identical(Purity$type,"twoing")) {
        return(abs.sum(IL, IR, p))
    } else {
        return(get.gain(Purity$call(Y), IL, IR, p))
    }
}

gain.ratio <- function (Purity, Y, Rcandidates, ...) UseMethod("gain.ratio")
gain.ratio.Purity <- function (Purity, Y, Rcandidates, ...) {
    p <- sum(Rcandidates)/length(Rcandidates)
    g <- gain(Purity, Y, Rcandidates)
    g/potential(p)
}

INFO <- function (Ys, ...) {
    Ys <- droplevels(Ys)
    p  <- table(Ys)/length(Ys)
    -1*sum(p*log(p))
}

GINI <- function (Ys, ...) {
    Ys <- droplevels(Ys)
    p <- table(Ys)/length(Ys)
    1 - sum(p^2)
}

TWOING <- function (Ys, ...) {
     table(Ys)/length(Ys)
}

potential <- function (p, ...) {
    -(p * log(p) + (1-p) * log(1-p))
}

get.gain <- function (I, IL, IR, p, ...) {
   I - p * IR - (1-p) * IL
}

abs.sum <- function (IL, IR, p, ...) {
    p * (1-p) * sum(abs(IL - IR))^2 / 4
}

