require(nnet)
require(MASS)
require(e1071)
require(neuralnet)


Model <- function (split, tune = NULL, feature.space, Y, X, dist.y, m.x, ...) {
    args <- mget(names(formals()),sys.frame(sys.nframe()))

    filter     <- do.call(toupper(feature.space$window), c(x.n = ncol(X), feature.space))
    cplx       <- do.call(Complexity, c(args, filter = filter))
    extractors <- lookup.features(feature.space$features)
    presets    <- lookup.presets(split, cplx)
    pcall      <- pre.call(split, tune, presets, filter, extractors)
    ppredict   <- pre.predict(split, filter, extractors)

    data <- list(
                 type      = split,
                 tuned     = tune,
                 presets   = presets,
                 filter    = filter,
                 extrators = extractors,
                 call      = pcall,
                 predictor = ppredict
                 )
    class(data) <- append(class(data), "Model")
    return(data)
}

Complexity <- function(Y, X, dist.y, m.x, feature.space, filter, ...) {

    m   <- nrow(X)
    n   <- ncol(X)
    u.y <- unique(droplevels(Y))
    t.y <- table(droplevels(Y))

    feats <- length(unlist(strsplit(feature.space$features, ":")))

    if (feature.space$window == "kbars") {
        f.vect <- feature.space$k * feats # k > 1
    } else {
        f.vect <- length(unlist(filter))

    }

    data <- list(x.m       = m,
                 x.n       = n,
                 c.n       = length(u.y),
                 o.c.prop  = length(u.y)/length(names(dist.y)),
                 t.prop    = m/m.x,
                 c.prop    = t.y/ unlist(lapply(u.y, function(x) dist.y[x])),
                 f.prop    = f.vect/n,
                 f.n       = f.vect
                 )
    class(data) <- append(class(data), "Complexity")
    return(data)
}

lookup.features <- function(features, ...) {
    f.list <- list("i" = I,
                   "s" = sum,
                   "m" = mean,
                   "e" = sd,
                   "k" = kurtosis,
                   "u" = skewness,
                   "a" = function(x) mean(abs(x)),
                   "d" = function(x) sum(x > c(0,x[-length(x)])) - 1,
                   "r" = function(x) sum(x < c(0,x[-length(x)])),
                   "o" = function(x) sum(x > (mean(x) + sd(x))) + sum(x < (mean(x) - sd(x))),
                   "l" = function(x) sum(abs(x[2:length(x)] - x[-length(x)])),
                   "f" = function(x) sum((x[2:length(x)] - x[-length(x)])^2),
                   "p" = function(x) {y <- (x - c(0,x[-length(x)]))[-c(1)]; if (sum(y>0) == 0) return(0) else mean(y[y > 0])},
                   "n" = function(x) {y <- (x - c(0,x[-length(x)]))[-c(1)]; if (sum(y<0) == 0) return(0) else mean(y[y < 0])},
                   "q" = function(x) {y <- (x - c(0,x[-length(x)]))[-c(1)]; if (sum(y>0) == 0) return(0) else sd(y[y > 0])},
                   "w" = function(x) {y <- (x - c(0,x[-length(x)]))[-c(1)]; if (sum(y<0) == 0) return(0) else sd(y[y < 0])}
                   )
    decode <- unlist(strsplit(features, ":"))
    f.list[names(f.list) %in% decode]
}

lookup.presets <- function (model, cplx, ...) {
    preset.list <- list("net" = list(size     = cplx$x.n,
                                     trace    = FALSE,
                                     decay    = 5e-4),
                                     #Wts      = pre.wts(Y.hat, X, n,length(r))),
                        "rnet" = list(size    = rNET(cplx)$size,
                                      MaxNWts = 15000,
                                      trace   = FALSE,
                                      decay   = 5e-4),
                                      #Wts     = pre.wts(Y.hat, X, n,length(r))),
                        "rmlp" = list(hidden  = rMLP(cplx)$n.units,
                                      act.fct = "logistic",
                                      rep     = 5,
                                      linear.output = FALSE),
                        "mlp"  = list(hidden  = c(5,5),
                                      act.fct = "logistic",
                                      rep     = 3,
                                      linear.output = FALSE),
                        "svm" = list(),
                        "lda" = list(),
                        "glm" = list(family = "gaussian"),
                        "lm"  = list())
    preset.list[[model]]
}

quick.p <- function (x.n, f.n, t.prop) {
    k <- round(x.n*1.5,0) - f.n
    p <- (k+1):1/(k+1)
    p <- p/sum(p)
    p * (1- t.prop) + p[length(p):1] * t.prop
}

rNET <- function(cplx) {
    f.n      <- cplx$f.n
    x.n      <- cplx$x.n
    t.prop   <- cplx$t.prop
    o.c.prop <- cplx$o.c.prop

    if (f.n == x.n) {
        size <- sample(seq(round(f.n * (.5 + o.c.prop),0), round(f.n*(.5 + 1.25),0)),1)
    } else {
        size <- sample(seq(f.n, round(x.n*1.5,0)),1, prob = quick.p(x.n, f.n, t.prop))
    }
    data <- list(size = size
                 )
    class(data) <- append(class(data), "rNET")
    return(data)
}


rMLP <- function(cplx) {
    c.n      <- cplx$c.n
    o.c.prop <- cplx$o.c.prop
    f.n      <- cplx$f.n
    x.n      <- cplx$x.n
    t.prop   <- cplx$t.prop

    layers      <- if(c.n == 2) 1 else max(round(o.c.prop * 2,0), 1)
    mine        <- max(round(f.n * 1/2,0),2)
    maxe        <- max(round(f.n * (1/2) + x.n * t.prop, 0),2)
    a           <- seq(mine, maxe)
    layer.units <- sample(c(a), layers, replace = TRUE)

    data <- list(n.layers  = layers,
                 n.units   = layer.units
                 )
    class(data) <- append(class(data), "rMLP")
    return(data)
}

ALL <- function(x.n, ...) {
    list(seq(x.n))
}

DOTS <- function(x.n, ...) {
    if (x.n == 1) {
        return(list(x.n))
    } else if (x.n == 2) {
        a <- sample(x.n, 1)
        return(list(sample(x.n, a, replace = FALSE )))
    } else {
        return(list(sample(x.n, round(x.n/2, 0), replace = FALSE )))
    }
}

BARS <- function(x.n, low = 1, high = x.n, ...) {
    l   <- sample(seq(low,high, 1), 1)
    bar <- 1:l
    p   <- sample(seq(0,x.n-l), 1)

    list(bar + p)
}

KBARS <- function(w, k, low = 1, high = w, ...) {
    print(c(w, k, low, high))
    lapply(c(0,seq(k-1)), function(x) c(x*w + unlist(BARS(w, low, high))))
}

random.box <- function(X, filter, extractors) {
    f <- lapply(filter, function(x) X[, x, drop = FALSE])
    f <- lapply(extractors, function(x) do.call(cbind, Map(function(y) apply(y, 1, x), f)))
    data.frame(do.call("cbind", f))
}

run.tuners <- function(tune) {
    if ("size" %in% names(tune)) {
        tune[["size"]] = tune[["size"]]()
    }
    if ("degree" %in% names(tune)){
        tune[["degree"]] = tune[["degree"]]()
    }
    if ("hidden" %in% names(tune)){
        tune[["hidden"]] = tune[["hidden"]]()
    }
    tune
}

pre.call <- function (model, tune, presets, filter, extractors,  ...) {
    if (is.null(tune)) {
        params <- force(presets)
    } else {
        tune <- run.tuners(tune)
        params <- force(tune)
    }
    if ("i" %in% names(extractors)) {
        mod <- function(Y, X, r, ...) {
            do.call(toupper(model), list(Y = Y, X = X[,unlist(filter), drop = FALSE], r = r, params = params))
        }
    } else {
        mod <- function(Y, X, r, ...) {
            mm.dat <- random.box(X, filter, extractors)
            print(ncol(mm.dat))
            do.call(toupper(model), list(Y = Y, X = mm.dat, r = r, params = params))
        }
    }
}

pre.predict <- function(model, filter, extractors, ...) {
    name <- force(paste0(toupper(model),".predict"))

    if ("i" %in% names(extractors)) {
        predictor <- function(n, X, ...) {
            do.call(name, list(n = n, X = X[,unlist(filter), drop = FALSE]))
        }
    } else {
        predictor <- function(n, X, ...) {
            mm.dat <- random.box(X, filter, extractors)
            do.call(name, list(n = n, X = mm.dat))
        }
    }
}

tune <- function (model, tuners, ...) UseMethod("tune")
tune.Model <- function (model, tuners, ...) {
    model$tuned <- tuners
    model
}

set.call <- function (model, ...) UseMethod("set.call")
set.call.Model <- function (model, ...) {
    if (is.null(model$tuned)) {
        params <- force(model$presets)
    } else {
        params <- force(model$tuned)
    }

    model$call <- function(Y, X, r, ...) {
        do.call(toupper(model$type), list(Y = Y, X = X, r = r, params = params))
    }
}

classEx <- function (Y, r, ...) {
    unlist(lapply(Y, function(x) x %in% r))
}

NET <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)

    forms <- list(formula = as.formula("Y.hat ~ ."),
                  data = X)
    #w <- pre.wts(Y.hat, X, params$size, 1)
    #do.call("nnet", c(forms, params, list(Wts = w)))
    do.call("nnet", c(forms, params))
}

RNET <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)
    forms <- list(formula = as.formula("Y.hat ~ ."),
                  data = X)
    #w <- pre.wts(Y.hat, X, params$size, 1)
    #do.call("nnet", c(forms, params, list(Wts = w)))
    do.call("nnet", c(forms, params))
}

MLP <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)
    Y.hat <- as.numeric(Y.hat)


    dat   <- cbind(l = Y.hat, X)
    forms <- list(formula = as.formula(paste("l ~ ", paste(colnames(X), collapse = "+"), collapse = "")),
                  data = dat)

    do.call("neuralnet", c(forms, params))
}

RMLP <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)
    Y.hat <- as.numeric(Y.hat)


    dat   <- cbind(l = Y.hat, X)
    forms <- list(formula = as.formula(paste("l ~ ", paste(colnames(X), collapse = "+"), collapse = "")),
                  data = dat)

    do.call("neuralnet", c(forms, params))
}

GLM <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)

    forms <- list(formula = as.formula("Y.hat ~ ."),
                  data = X)

    do.call("glm", c(forms, params))
}

SVM <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)
    Y.hat <- as.factor(Y.hat)

    forms <- list(formula = as.formula("Y.hat ~ ."),
                  data = X)

    do.call("svm", c(forms, params))
}

LDA <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)

    forms <- list(formula = as.formula("Y.hat ~ ."),
                  data = X)

    do.call("lda", c(forms, params))
}

LM <- function (Y, X, r, params, ...) {
    Y.hat <- classEx(Y,r)

    forms <- list(formula = as.formula("Y.hat ~ ."),
                  data = X)

    do.call("lm", c(forms, params))
}

NET.predict <- function(n, X) {
    preds <- c(predict(n, X))
    preds > .5
}

RNET.predict <- function(n, X) {
    preds <- c(predict(n, X))
    preds > .5
}

MLP.predict <- function(n, X) {
    preds <- compute(n, X)$net.result
    preds > .5
}

RMLP.predict <- function(n, X) {
    preds <- compute(n, X)$net.result
    preds > .5
}

GLM.predict <- function(n, X) {
    preds <- c(predict(n, X))
    preds > .5
}

LM.predict <- function(n, X) {
    preds <- c(predict(n, X))
    preds > .5
}

SVM.predict <- function(n, X) {
    as.logical(as.vector(predict(n,X)))
}

LDA.predict <- function(n, X) {
    c(predict(n, X)[["class"]])
}

