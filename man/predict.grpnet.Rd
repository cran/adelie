% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{predict.grpnet}
\alias{predict.grpnet}
\alias{coef.grpnet}
\title{make predictions from a "grpnet" object.}
\usage{
\method{predict}{grpnet}(
  object,
  newx,
  lambda = NULL,
  type = c("link", "response", "coefficients"),
  newoffsets = NULL,
  ...
)

\method{coef}{grpnet}(object, lambda = NULL, ...)
}
\arguments{
\item{object}{Fitted \code{"grpnet"} model.}

\item{newx}{Matrix of new values for \code{x} at which predictions are to be
made. Can be a matrix, a sparse matrix as in \code{Matrix} package, or else any of the matrix forms
allowable in the \code{adelie} package. The number of columns must match that of the input matrix
used in fitting \code{object}. If the model object was fit with \code{standardize=TRUE}, the saved
centers and scaling will be applied to this matrix. This argument is not used for \code{type="coefficients"}}

\item{lambda}{Value(s) of the penalty parameter \code{lambda} at which
predictions are required. Default is the entire sequence used to create the
model. If values of \code{lambda} are supplied, the function uses linear
interpolation to make predictions for values of \code{lambda} that do
not coincide with those used in the fitting algorithm.}

\item{type}{Type of prediction required. Type \code{"link"} is  the default, and gives the linear
predictors. Type \code{"response"} applies the inverse link to these predictions.
Type \code{"coefficients"} extracts the coefficients, intercepts and the active-set sizes.}

\item{newoffsets}{If an offset is used in the fit, then one must be supplied
for making predictions (except for \code{type="coefficients"} or
\code{type="nonzero"})}

\item{\dots}{Currently ignored.}
}
\value{
The object returned depends on type.
}
\description{
Similar to other predict methods, this functions predicts linear predictors,
coefficients and more from a fitted \code{"grpnet"} object.
}
\details{
The shape of the objects returned are different for \code{"multinomial"} and \code{"multigaussian"}
objects
\code{coef(...)} is equivalent to \code{predict(type="coefficients",...)}
}
\examples{
set.seed(0)
n <- 100
p <- 200
X <- matrix(rnorm(n * p), n, p)
y <- X[,1] * rnorm(1) + rnorm(n)
fit <- grpnet(X, glm.gaussian(y))
coef(fit)
predict(fit,newx = X[1:5,])
}
\references{
Yang, James and Hastie, Trevor. (2024) A Fast and Scalable Pathwise-Solver for Group Lasso
and Elastic Net Penalized Regression via Block-Coordinate Descent. arXiv \doi{10.48550/arXiv.2405.08631}.\cr
Adelie Python user guide  \url{https://jamesyang007.github.io/adelie/}
}
\seealso{
\code{grpnet}, and \code{print}, and \code{coef} methods, and
\code{cv.grpnet}.
}
\author{
James Yang, Trevor Hastie, and  Balasubramanian Narasimhan \cr Maintainer: Trevor Hastie
\email{hastie@stanford.edu}
}
\keyword{models}
\keyword{regression}
