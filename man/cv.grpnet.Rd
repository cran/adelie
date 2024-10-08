% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{cv.grpnet}
\alias{cv.grpnet}
\title{Cross-validation for grpnet}
\usage{
cv.grpnet(
  X,
  glm,
  n_folds = 10,
  foldid = NULL,
  min_ratio = 0.01,
  lmda_path_size = 100,
  offsets = NULL,
  progress_bar = FALSE,
  n_threads = 1,
  ...
)
}
\arguments{
\item{X}{Feature matrix. Either a regualr R matrix, or else an
\code{adelie} custom matrix class, or a concatination of such.}

\item{glm}{GLM family/response object. This is an expression that
represents the family, the reponse and other arguments such as
weights, if present. The choices are \code{glm.gaussian()},
\code{glm.binomial()}, \code{glm.poisson()},
\code{glm.multinomial()}, \code{glm.cox()}, \code{glm.multinomial()},
and \code{glm.multigaussian()}. This is a required argument, and
there is no default. In the simple example below, we use \code{glm.gaussian(y)}.}

\item{n_folds}{(default 10). Although \code{n_folds} can be
as large as the sample size (leave-one-out CV), it is not recommended for
large datasets. Smallest value allowable is \code{n_folds=3}.}

\item{foldid}{An optional vector of values between 1 and \code{n_folds}
identifying what fold each observation is in. If supplied, \code{n_folds} can
be missing.}

\item{min_ratio}{Ratio between smallest and largest value of lambda. Default is 1e-2.}

\item{lmda_path_size}{Number of values for \code{lambda}, if generated automatically.
Default is 100.}

\item{offsets}{Offsets, default is \code{NULL}. If present, this is
a fixed vector or matrix corresponding to the shape of the natural
parameter, and is added to the fit.}

\item{progress_bar}{Progress bar. Default is \code{FALSE}.}

\item{n_threads}{Number of threads, default \code{1}.}

\item{\dots}{Other arguments that can be passed to \code{grpnet}}
}
\value{
an object of class \code{"cv.grpnet"} is returned, which is a list
with the ingredients of the cross-validation fit.
\item{lambda}{the values of \code{lambda} used in the
fits.}
\item{cvm}{The mean cross-validated deviance - a vector of length \code{length(lambda)}.}
\item{cvsd}{estimate of standard error of \code{cvm}.}
\item{cvup}{upper curve = \code{cvm+cvsd}.}
\item{cvlo}{lower curve = \code{cvm-cvsd}.}
\item{nzero}{number of non-zero coefficients at each \code{lambda}.}
\item{name}{a text string indicating type of measure (for plotting purposes).
Currently this is \code{"deviance"}}
\item{grpnet.fit}{a fitted grpnet object for the
full data.}
\item{lambda.min}{value of \code{lambda} that gives minimum \code{cvm}.}
\item{lambda.1se}{largest value of \code{lambda} such that
mean deviance is within 1 standard error of the minimum.}
\item{index}{a one column matrix with the indices of \code{lambda.min} and \code{lambda.1se} in the sequence of coefficients, fits etc.}
}
\description{
Does k-fold cross-validation for grpnet
}
\details{
The function runs \code{grpnet} \code{n_folds}+1 times; the first to get the
\code{lambda} sequence, and then the remainder to compute the fit with each
of the folds omitted. The out-of-fold deviance is accumulated, and the average deviance and
standard deviation over the folds is computed.  Note that \code{cv.grpnet}
does NOT search for values for \code{alpha}. A specific value should be
supplied, else \code{alpha=1} is assumed by default. If users would like to
cross-validate \code{alpha} as well, they should call \code{cv.grpnet} with
a pre-computed vector \code{foldid}, and then use this same \code{foldid} vector in
separate calls to \code{cv.grpnet} with different values of \code{alpha}.
Note also that the results of \code{cv.grpnet} are random, since the folds
are selected at random. Users can reduce this randomness by running
\code{cv.grpnet} many times, and averaging the error curves.
}
\examples{
set.seed(0)
n <- 100
p <- 200
X <- matrix(rnorm(n * p), n, p)
y <- X[,1] * rnorm(1) + rnorm(n)
fit <- grpnet(X, glm.gaussian(y))
print(fit)

}
\references{
Yang, James and Hastie, Trevor. (2024) A Fast and Scalable Pathwise-Solver for Group Lasso
and Elastic Net Penalized Regression via Block-Coordinate Descent. arXiv \doi{10.48550/arXiv.2405.08631}.\cr
Friedman, J., Hastie, T. and Tibshirani, R. (2008)
\emph{Regularization Paths for Generalized Linear Models via Coordinate
Descent (2010), Journal of Statistical Software, Vol. 33(1), 1-22},
\doi{10.18637/jss.v033.i01}.\cr
Simon, N., Friedman, J., Hastie, T. and Tibshirani, R. (2011)
\emph{Regularization Paths for Cox's Proportional
Hazards Model via Coordinate Descent, Journal of Statistical Software, Vol.
39(5), 1-13},
\doi{10.18637/jss.v039.i05}.\cr
Tibshirani,Robert, Bien, J., Friedman, J., Hastie, T.,Simon, N.,Taylor, J. and
Tibshirani, Ryan. (2012) \emph{Strong Rules for Discarding Predictors in
Lasso-type Problems, JRSSB, Vol. 74(2), 245-266},
\url{https://arxiv.org/abs/1011.2234}.\cr
}
\author{
James Yang, Trevor Hastie, and  Balasubramanian Narasimhan \cr Maintainer: Trevor Hastie
\email{hastie@stanford.edu}
}
