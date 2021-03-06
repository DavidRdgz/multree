% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/tree.R
\name{multree}
\alias{multree}
\title{Building a multivariate decision tree}
\usage{
multree(Y, X, split = "svm", a = 0.8, tune = NULL, purity = "gini",
  feature.space = list(window = "all", w = 0, k = 0, features = "i"), ...)
}
\arguments{
\item{Y}{a vector of classes set as factors.}

\item{X}{a dataframe of continuous or categorical values.}

\item{split}{may be: \code{glm, svm, net, lm, lda}. Sets the hyper surface spliter at nodes in the tree.}

\item{a}{may be: any real value. Cuts-off tree growth if subset has purity greater than \code{a}.}

\item{tune}{may be a list of arguments used instead of presets for \code{glm, svm, net, lm, lda}.}

\item{purity}{may be: \code{gini, information, twoing}. Sets impurity measure for a node.}

\item{feature.space}{List identifying the columns of the matrix, and features to extract using \code{window, w, k, features} variables.}

\item{window}{maybe be: \code{all,dots,bars,kbars}. This provides at each split, either selecting all the columns (all) or randomly selecting #(cols/2) (dots), or selecting a random sequence of columns (bars).}

\item{w}{may be: used with \code{kbars} specifies window size.}

\item{k}{may be: used with \code{kbars} specifies the slide (number of \code{w} windows).}

\item{features}{may be: \code{i, s, m, a}. These represent: i = identity, s = sum, m = mean, a = aboslute mean aggregates over filter columns.}
}
\value{
an MulTree object with a tree consisting of nodes and their attributes.
}
\description{
This function develops a multivariate decision tree classifier in 'x' to the class of 'y'.
}
\details{
This function develops a multivariate decision tree classifier to predict an unseen 'x's class 'y'. The options of using a generalized linear model, support vector machine, neural network, or linear model to construct a hyper-surface splitting plane in ways not found in decision trees.
}
\examples{
Y  <- iris[,5]
X  <- iris[,1:4]
dt <- multree(Y, X, "svm")
}
\author{
David Rodriguez
}
\seealso{
\code{glm, svm, nnet, lm, lda}
}

