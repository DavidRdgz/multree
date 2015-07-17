# multree
Is a multivariate decision tree classifier. This classifier constructs a decision tree where
the splitting hypersurface are not necessarily orthogonal hyperplanes. 

# Why
A multivariate decision tree classifier addresses the problems traditionally encountered with decision trees. Some issues that
arise with traditional decision trees are

- Large tree sizes
- Over-Under fitting
- Class dependency

# Features
Probably the most important feature of this package is the variety of multivariate splitting criterias. For example, you can choose from

- Generalized linear models
- Support vector machines
- Neural networks
- Linear discriminant analysis
- Linear models

The performance of the multivariate decision tree fitted from one of these methods will depend on your use case.

# Plots

An important part of analysing decision trees is to look at their structure. This package utilizes the networkD3 package
to give an insight into the majority of the class and their percentage of each node in the tree.


