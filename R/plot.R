source("R/tree.R")
require(networkD3)

lookup <- function(labels, unique.labels) {
  for (iter in 1:length(unique.labels)) {
      labels[labels == unique.labels[iter]] <- iter
      }
  as.numeric(labels)
}

get.children.id <- function (node, ...) {
    c(node$l.id, node$r.id)
}

get.labels <- function (dt, ...) {
    names<- c()
    labs <- c()
    for (node in dt$tree) {
        names <- c(names, paste(node$id, "-", sort(paste(node$r, collapse="-")),"-",node$percent))
        labs <- c(labs, sort(paste(node$r, collapse="-")))
    }
    candidates <- unique(labs)
    data.frame(names, group = labs, size = rep(1,length(names)))
}

graph <- function (dt, ...) {
    Source <- c()
    Target <- c()
    for (node in dt$tree) {
        if (!is.a.leaf(node)) {
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

#' A D3 graph of a multivariate decision tree
#'
#' This function provides an interactive interface to analyze a \code{multree} object.
#'
#' @param dt is \code{multree} fitted object.
#' @return an interactive D3 tree object with labels.
#' @author David Rodriguez
#' @details
#' This function builds a D3 javascript object to view and analyze a \code{multree} object. Presently there are no options to alter labels.
#'
#' @seealso \code{networkD3}
#'
#' @examples
#' Y  <- iris[,5]
#' X  <- iris[,1:4]
#' dt <- multree(Y,X)
#' force.graph(dt)

force.graph <- function (dt, ...) {
    networkD3::forceNetwork(Links = graph.value(dt), Nodes = get.labels(dt),
                Source = "Source", Target = "Target", Value = "value",
                NodeID = "names", Group = "group", opacity = .9,
                legend = TRUE, zoom = TRUE, fontSize = 10,
                opacityNoHover = 1)
}
