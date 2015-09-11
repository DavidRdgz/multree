require(nnet)
require(MASS)
require(e1071)

Model <- function (type = "net", tune = NULL, window = "all", features = "i", n) {

    preset    <- presets(type)
    filter    <- pre.filter(window, n)
    extractors<- pre.extractors(features)
    pcall     <- pre.call(type, preset, tune, filter, features)
    ppredict  <- pre.predict(type, filter, features)

    data <- list(
                 type      = type,
                 tuned     = tune,
                 presets   = preset,
                 filter    = filter,
                 extrators = extractors,
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

pre.extractors <- function(features, ...) {
    f.list <- list("i" = I,
                   "s" = sum,
                   "m" = mean,
                   "a" = function(x) mean(abs(x)))
    decode <- unlist(strsplit(features, "*[w]*"))
    f.list[names(f.list) %in% decode]
}

ALL <- function(n, ...) {
  list(seq(n))
}

DOTS <- function(n, ...) {
  list(sample(seq(n), floor(n/2), replace = FALSE ))
}

BARS <- function(n, ...) {
  l <- sample(seq(n),1)
  if (l == n) {
    return(list(seq(l)))
  } else {
    s <- sample(c(0,seq(n-l)),1)
    return(list(seq(l) + s))
  }
}

random.box <- function(X, filter, extractors) {
    f <- lapply(filter, function(x) X[, x, drop = FALSE])
    f <- lapply(extractors, function(x) do.call(cbind, Map(function(y) apply(y, 1, x), f)))
    do.call(cbind, f)
}

pre.call <- function (type, presets, tune, filter, features,  ...) {
    if (is.null(tune)) {
        params <- force(presets)
    } else {
        params <- force(tune)
    }

    if (features == "i") {
      model <- function(Y, X, r, ...) {
          do.call(toupper(type), list(Y = Y, X = X[,unlist(filter), drop = FALSE], r = r, params = params))
      }
    } else {
      model <- function(Y, X, r, ...) {
          do.call(toupper(type), list(Y = Y, X = random.box(X, filter, features), r = r, params = params))
      }
    }
}

pre.predict <- function(type, filter, features, ...) {
    name <- force(paste0(toupper(type),".predict"))

    if (features == "i") {
      predictor <- function(n, X, ...) {
          do.call(name, list(n = n, X = X[,unlist(filter), drop = FALSE]))
      }
    } else {
      predictor <- function(n, X, ...) {
          do.call(name, list(n = n, X = random.box(X, filter, features)))
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

