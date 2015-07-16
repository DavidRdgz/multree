source("data.R")
source("tree.R")
source("forest.R")
source("plot.R")
library(caret)



###
#
# Get either letter or iris data
d    <- get.iris()

###
#
# Set the number of K-folds for validation
k    <- 4
flds <- createFolds(1:length(d[["Y"]]), k = k, list = TRUE, returnTrain = FALSE)

###
#
# Run K-folds and compute confusion matrices (accuracy along diagonals)
rez  <- lapply(1:k, function(x) kfolds.accuracy(d, flds, k, x))



