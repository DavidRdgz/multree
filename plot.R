source("tree.R")
library(networkD3)

get.children.id <- function (node, ...) {
    c(node$l.id, node$r.id)
}

rep.parent <- function (parent, children, ...) {
    rep(parent, length(children))
}

get.labels <- function (dt, ...) {
    names<- c()
    for (node in dt$tree) {
        names <- c(names, paste(node$id, "-", node$label,"-",node$percent))
    }
    data.frame(names, group = 1:length(names), size = rep(1,length(names)))
}

graph <- function (dt, ...) {
    Source <- c()
    Target <- c()
    for (node in dt$tree) {
        if (!is.leaf(node)) {
            Source <- c(Source, rep(node$id, 2)) 
            Target <- c(Target, get.children.id(node))
        }
    }
    data.frame(Source, Target)
}

graph.value <- function (dt, ...) {
    data.frame(graph(dt)-1, value = rep(1,nrow(graph(dt))))
}

simple.graph <- function (dt, ...) {
    simpleNetwork(graph(dt))
}

force.graph <- function (dt, ...) {
    forceNetwork(Links = graph.value(dt), Nodes = get.labels(dt),
                Source = "Source", Target = "Target", Value = "value",
                NodeID = "names", Group = "group", opacity = .8, 
                legend = TRUE, zoom = TRUE, fontSize = 10)
}
