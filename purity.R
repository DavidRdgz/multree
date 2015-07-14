Info <- function (Ys, ...) {
    Ys <- droplevels(Ys)
    p  <- table(Ys)/length(Ys)
    -1*sum(p*log(p))
}

Gini <- function (Ys, ...) {
    Ys <- droplevels(Ys)
    p <- table(Ys)/length(Ys)
    1 - sum(p^2)
}

Twoing <- function (Ys, ...) {
     table(Ys)/length(Ys)
}

Potential <- function (p, ...) {
    -(p * log(p) + (1-p) * log(1-p))
}

Gain <- function (I, IL, IR, p, ...) {
   I - p * IR - (1-p) * IL
}

AbsSum <- function (IL, IR, p, ...) {
    p * (1-p) * sum(abs(IL - IR))^2 / 4
}

