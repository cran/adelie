% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solver.R
\name{grpnet}
\alias{grpnet}
\title{fit a GLM with group lasso or group elastic-net regularization}
\usage{
grpnet(
  X,
  glm,
  constraints = NULL,
  groups = NULL,
  alpha = 1,
  penalty = NULL,
  offsets = NULL,
  lambda = NULL,
  standardize = TRUE,
  irls_max_iters = as.integer(10000),
  irls_tol = 1e-07,
  max_iters = as.integer(1e+05),
  tol = 1e-07,
  adev_tol = 0.9,
  ddev_tol = 0,
  newton_tol = 1e-12,
  newton_max_iters = 1000,
  n_threads = 1,
  early_exit = TRUE,
  intercept = TRUE,
  screen_rule = c("pivot", "strong"),
  min_ratio = 0.01,
  lmda_path_size = 100,
  max_screen_size = NULL,
  max_active_size = NULL,
  pivot_subset_ratio = 0.1,
  pivot_subset_min = 1,
  pivot_slack_ratio = 1.25,
  check_state = FALSE,
  progress_bar = FALSE,
  warm_start = NULL
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

\item{constraints}{Constraints on the parameters. Currently these are ignored.}

\item{groups}{This is an ordered vector of integers that represents the groupings,
with each entry indicating where a group begins. The entries refer to column numbers
in the feature matrix.
If there are \code{p} features, the default is \code{1:p} (no groups).
(Note that in the output of \code{grpnet} this vector might be shifted to start from 0,
since internally \code{adelie} uses zero-based indexing.)}

\item{alpha}{The elasticnet mixing parameter, with \eqn{0\le\alpha\le 1}.
The penalty is defined as
\deqn{(1-\alpha)/2\sum_j||\beta_j||_2^2+\alpha\sum_j||\beta_j||_2,} where thte sum is over groups.
\code{alpha=1} is pure group
lasso penalty, and \code{alpha=0} the pure ridge penalty.}

\item{penalty}{Separate penalty factors can be applied to each group of coefficients.
This is a number that multiplies \code{lambda} to allow
differential shrinkage for groups. Can be 0 for some groups, which implies no
shrinkage, and that group is always included in the model.
Default is square-root of group sizes for each group.}

\item{offsets}{Offsets, default is \code{NULL}. If present, this is
a fixed vector or matrix corresponding to the shape of the natural
parameter, and is added to the fit.}

\item{lambda}{A user supplied \code{lambda} sequence. Typical usage is to
have the program compute its own \code{lambda} sequence based on
\code{lmda_path_size} and \code{min_ratio}.}

\item{standardize}{If \code{TRUE} (the default), the columns of \code{X} are standardized before the
fit is computed. This is good practice if the features are a mixed bag, because it has an impact on
the penalty. The regularization path is computed using the standardized features, and the
standardization information is saved on the object for making future predictions.}

\item{irls_max_iters}{Maximum number of IRLS iterations, default is
\code{1e4}.}

\item{irls_tol}{IRLS convergence tolerance, default is \code{1e-7}.}

\item{max_iters}{Maximum total number of coordinate descent
iterations, default is \code{1e5}.}

\item{tol}{Coordinate descent convergence tolerance, default \code{1e-7}.}

\item{adev_tol}{Fraction deviance explained tolerance, default
\code{0.9}. This can be seen as a limit on overfitting the
training data.}

\item{ddev_tol}{Difference in fraction deviance explained
tolerance, default \code{0}. If a step in the path changes the
deviance by this amount or less, the algorithm truncates the
path.}

\item{newton_tol}{Convergence tolerance for the BCD update, default
\code{1e-12}. This parameter controls the iterations in each
block-coordinate step to establish the block solution.}

\item{newton_max_iters}{Maximum number of iterations for the BCD
update, default \code{1000}.}

\item{n_threads}{Number of threads, default \code{1}.}

\item{early_exit}{\code{TRUE} if the function should be allowed to exit
early.}

\item{intercept}{Default \code{TRUE} to include an unpenalized
intercept.}

\item{screen_rule}{Screen rule, with default \code{"pivot"}. Other option is \code{"strong"}.
(an empirical improvement over \code{"strong"}, the other option.)}

\item{min_ratio}{Ratio between smallest and largest value of lambda. Default is 1e-2.}

\item{lmda_path_size}{Number of values for \code{lambda}, if generated automatically.
Default is 100.}

\item{max_screen_size}{Maximum number of screen groups. Default is \code{NULL}.}

\item{max_active_size}{Maximum number of active groups. Default is \code{NULL}.}

\item{pivot_subset_ratio}{Subset ratio of pivot rule. Default is \code{0.1}. Users not expected to fiddle with this.}

\item{pivot_subset_min}{Minimum subset of pivot rule. Defaults is \code{1}. Users not expected to fiddle with this.}

\item{pivot_slack_ratio}{Slack ratio of pivot rule, default is \code{1.25}. Users not expected to fiddle with this.
See reference for details.}

\item{check_state}{Check state. Internal parameter, with default \code{FALSE}.}

\item{progress_bar}{Progress bar. Default is \code{FALSE}.}

\item{warm_start}{Warm start (default is \code{NULL}). Internal parameter.}
}
\value{
A list of class \code{"grpnet"}. This has a main component called \code{state} which
represents the fitted path, and a few extra
useful components such as the \code{call}, the \code{family} name, and \code{group_sizes}.
Users typically use methods like \code{predict()}, \code{print()}, \code{plot()} etc to examine the object.
}
\description{
Computes a group elastic-net regularization path for a variety of
GLM and other families, including the Cox model. This function
extends the abilities of the \code{glmnet} package to allow for
grouped regularization. The code is very efficient (core routines
are written in C++), and allows for specialized matrix
classes.
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
\seealso{
\code{cv.grpnet}, \code{predict.grpnet}, \code{plot.grpnet}, \code{print.grpnet}.
}
\author{
James Yang, Trevor Hastie, and  Balasubramanian Narasimhan \cr Maintainer: Trevor Hastie
\email{hastie@stanford.edu}
}
