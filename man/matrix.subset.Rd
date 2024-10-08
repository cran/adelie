% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{matrix.subset}
\alias{matrix.subset}
\title{Creates a subset of the matrix along an axis.}
\usage{
matrix.subset(mat, indices, axis = 1, n_threads = 1)
}
\arguments{
\item{mat}{The \code{adelie} matrix to subset.}

\item{indices}{Vector of indices to subset the matrix.}

\item{axis}{The axis along which to subset (2 is columns, 1 is rows).}

\item{n_threads}{Number of threads.}
}
\value{
Matrix subsetted along the appropriate axis.
The object is an S4 class with methods for efficient computation by adelie.
}
\description{
Creates a subset of the matrix along an axis.
}
\examples{
n <- 100
p <- 20
X <- matrix.dense(matrix(rnorm(n * p), n, p))
indices <- c(1, 3, 10)
out <- matrix.subset(X, indices, axis=1)
out <- matrix.subset(X, indices, axis=2)
}
\author{
James Yang, Trevor Hastie, and  Balasubramanian Narasimhan \cr Maintainer: Trevor Hastie \href{mailto:hastie@stanford.edu}{hastie@stanford.edu}
}
