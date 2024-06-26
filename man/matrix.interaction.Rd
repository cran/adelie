% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{matrix.interaction}
\alias{matrix.interaction}
\title{Creates a matrix with pairwise interactions.}
\usage{
matrix.interaction(mat, intr_keys, intr_values, levels = NULL, n_threads = 1)
}
\arguments{
\item{mat}{The dense matrix.}

\item{intr_keys}{List of feature indices.}

\item{intr_values}{List of list of feature indices.}

\item{levels}{Levels.}

\item{n_threads}{Number of threads.}
}
\value{
Pairwise interaction matrix.
}
\description{
Creates a matrix with pairwise interactions.
}
\examples{
n <- 10
p <- 20
X_dense <- matrix(rnorm(n * p), n, p)
X_dense[,1] <- rbinom(n, 4, 0.5)
intr_keys <- c(0, 1)
intr_values <- list(NULL, c(0, 2))
levels <- c(c(5), rep(0, p-1))
out <- matrix.interaction(X_dense, intr_keys, intr_values, levels)
}
