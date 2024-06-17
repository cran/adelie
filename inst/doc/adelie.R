## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(adelie)

set.seed(0)
n <- 100
p <- 200
X <- matrix(rnorm(n * p), n, p)
y <- X[,1] * rnorm(1) + rnorm(n)
state <- grpnet(X, glm.gaussian(y))

