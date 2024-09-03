## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##",
  fig.width = 7,
  fig.height=6
)

## -----------------------------------------------------------------------------
library(adelie)

## -----------------------------------------------------------------------------
n = 100     # number of observations
p = 1000    # number of features
set.seed(5) # seed
X = matrix(rnorm(n*p),n,p)
y = X[,1:10]%*%rnorm(10) + rnorm(n)*sqrt(10) # makes SNR = 1

## -----------------------------------------------------------------------------
fit = grpnet(X=X, glm=glm.gaussian(y=y))
print(fit)

## -----------------------------------------------------------------------------
plot(fit)

## -----------------------------------------------------------------------------
pred = predict(fit, newx = X[1:5,],lambda = c(1.5, 1))
pred

## -----------------------------------------------------------------------------
fitcv = cv.grpnet(X,glm.gaussian(y),progress_bar = TRUE)
plot(fitcv)

## -----------------------------------------------------------------------------
fitcv

## -----------------------------------------------------------------------------
pred = predict(fitcv, newx = X)
pred = predict(fitcv, newx = X, lambda = "lambda.min")

## -----------------------------------------------------------------------------
fitg = grpnet(
    X=X,
    glm=glm.gaussian(y=y),
    groups=seq(from = 1, to = p, by=10),
    )
print(fitg)
plot(fitg)

## -----------------------------------------------------------------------------
fitgcv = cv.grpnet(
    X,
    glm.gaussian(y),
    groups=seq(from = 1, to = p, by=10),
    progress_bar = TRUE)
plot(fitgcv)

## -----------------------------------------------------------------------------
eta = X[,1:5]%*%rnorm(5)/sqrt(5)
mu = 1 / (1 + exp(-eta))
y = rbinom(n, size = 1, prob = mu)

## -----------------------------------------------------------------------------
fitb = grpnet(X, glm.binomial(y))
plot(fitb)

## -----------------------------------------------------------------------------
fitbcv = cv.grpnet(X,glm.binomial(y),progress_bar = TRUE)
plot(fitbcv)

## -----------------------------------------------------------------------------
n = 300     # number of observations
p = 100    # number of features
K = 4       # number of classes
set.seed(7)
X = matrix(rnorm(n*p),n,p)
eta = X[, 1:5] %*% matrix(rnorm(K*5),5,K)/sqrt(5)
probs = exp(eta)
probs = probs/rowSums(probs)
Y = t(apply(probs,1,function(prob)rmultinom(1, 1, prob)))
Y[1:5,]

## ----fitm---------------------------------------------------------------------
grps = seq(from=1, to=p, by = 5)
fitm = grpnet(X, glm.multinomial(Y), groups=grps)

## ----out.lines = 10-----------------------------------------------------------
print(fitm)

## ----plotfitm-----------------------------------------------------------------
plot(fitm)

## -----------------------------------------------------------------------------
fitmcv = cv.grpnet(X,glm.multinomial(Y),groups=grps)
plot(fitmcv)

## -----------------------------------------------------------------------------
names(coef(fitm))

## -----------------------------------------------------------------------------
predict(fitmcv,newx = X[1:3,])

## -----------------------------------------------------------------------------
set.seed(9)
n <- 500
p <- 100
X  <- matrix(rnorm(n*p), n, p)
X[sample.int(n * p, size = 0.5 * n * p)] <- 0
X_sparse <- Matrix::Matrix(X, sparse = TRUE)

nzc <- p / 4
beta <- rnorm(nzc)
fx <- X[, seq(nzc)] %*% beta / 3
hx <- exp(fx)
y <- rexp(n,hx)
status <- rbinom(n = n, prob = 0.3, size = 1)

## -----------------------------------------------------------------------------
groups = seq(from = 1, to = p, by = 5)
fitcv <- cv.grpnet(X_sparse,
                   glm.cox(stop = y, status = status),
                   alpha = 0.5,
                   groups = groups)
par(mfrow = c(1,2))
plot(fitcv)
plot(fitcv$grpnet.fit)

