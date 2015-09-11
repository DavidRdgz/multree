require(nnet)
require(MASS)
require(e1071)

Model <- function (type = "net", tune = NULL, window = "all", n) {

    preset   <- presets(type)
    filter   <- pre.filter(window, n)
    pcall    <- pre.call(type, preset, tune, filter)
    ppredict <- pre.predict(type, filter)

    data <- list(
                 type      = type,
                 tuned     = tune,
                 presets   = preset,
                 filter    = filter,
                 call      = pcall,
                 predictor = ppredict
                )
    class(data) <- append(class(data), "Model")
    return(data)
}

presets <- function (model, ...) {
    preset.list <- list("net" = list(size = 10,
                                     trace = FALSE),
                        "svm" = list(),
                        "lda" = list(),
                        "glm" = list(family = "gaussian"),
                        "lm"  = list())
    preset.list[[model]]
}

pre.filter <- function(window, n, ...) {
    do.call(toupper(window), list(n = n))
}

ALL <- function(n, ...) {
  seq(n)
}

DOTS <- function(n, ...) {
  sample(seq(n), floor(n/2), replace = FALSE )
}

BARS <- function(n, ...) {
  l <- sample(seq(n),1)
  s <- sample(seq(n-l),1)
  seq(l) + s
}

pre.call <- function (type, presets, tune, filter, ...) {
    if (is.null(tune)) {
        params <- force(presets)
    } else {
        params <- force(tune)
    }

    model <- function(Y, X, r, ...) {
        do.call(toupper(type), list(Y = Y, X = X[,filter, drop = FALSE], r = r, params = params))
    }
}

pre.predict <- function(type, filter, ...) {
    name <- force(paste0(toupper(type),".predict"))

    predictor <- function(n, X, ...) {
        do.call(name, list(n = n, X = X[,filter, drop = FALSE]))
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

    do.call("nnet", c(forms, params))
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

