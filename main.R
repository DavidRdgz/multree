source("tree.R")
source("forest.R")
source("plot.R")

####
#
# Pre-Bagging (assuming not random forest)

#s <- sample(1:nrow(iris), 200, replace = TRUE)
#X <- iris[s,1:4]; Y <- iris[s,5]


####
#
# No-Bagging
X <- iris[,1:4]; Y <- iris[,5]

####
#
# Fit decision tree

dt <- tree(X, Y, Pure = Info, model = GLM)
print(table(pred = m.predict(dt, iris[,1:4]), actu = iris[,5]))
force.graph(dt)

####
#
# Fit random forest

#rf <- forest(X, Y, 10, splitter = Branch, f =2)
#print(table(pred = rf.predict(rf, X), actu = Y))
