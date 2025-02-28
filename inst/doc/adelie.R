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
y = X[,1:10] %*% rnorm(10) + rnorm(n) * sqrt(10) # makes SNR = 1

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
eta = X[,1:5] %*% rnorm(5) / sqrt(5)
mu = 1 / (1 + exp(-eta))
y = rbinom(n, size = 1, prob = mu)

## -----------------------------------------------------------------------------
fitb = grpnet(X, glm.binomial(y))
plot(fitb)

## -----------------------------------------------------------------------------
fitbcv = cv.grpnet(X,glm.binomial(y))
plot(fitbcv)

## -----------------------------------------------------------------------------
predict(fitb, newx = X[1:5,],lambda = c(0.13, 0.07))

## -----------------------------------------------------------------------------
predict(fitb, newx = X[1:5,],lambda =  c(0.13, 0.07),type="response")

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
predict(fitmcv,newx = X[1:3,], lambda="lambda.min", type = "response")

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

## -----------------------------------------------------------------------------
n <- 100
p <- 30
set.seed(1)
X <- matrix(rnorm(n * p), n, p)
y <- X[,c(1:5)] %*% rnorm(5)/3 + rnorm(n)

fit <- grpnet(X, glm.gaussian(y))
constrs = lapply(1:p, function(i) constraint.box(lower = 0, upper = Inf))
fit.constr = grpnet(X, glm.gaussian(y), constraints = constrs)
par(mfrow=c(1,2))
plot(fit)
plot(fit.constr)

## -----------------------------------------------------------------------------
fit <- grpnet(X, glm.gaussian(y), groups=c(1,5:30))
beta <- coef(fit)$beta[100, 1:4]
print(beta)
lower = lower=c(-Inf,-Inf,-Inf,-.2)
upper=c(0.2,0.05,0.4,Inf)
constrs = rep(list(NULL),27) # there are 27 groups
constrs[[1]] = constraint.box(lower=lower,upper=upper)
fit.constr = grpnet(X, glm.gaussian(y), groups=c(1,5:30), constraints = constrs)
par(mfrow=c(1,2))
plot(fit)
plot(fit.constr)
abline(h=lower,col="blue",lty=2)
abline(h=upper,col="blue",lty=2)

## ----dense--------------------------------------------------------------------
n = 4
p = 2
set.seed(0)
X_dense = matrix(rnorm(n*p),n,p)
print(X_dense)
Xd = matrix.dense(X_dense)
print(Xd)

## ----sparse-------------------------------------------------------------------
d = data.frame(
    i = c(2, 1, 4, 1, 2, 4),
    j= c(1, 2, 2, 3, 3, 3),
    x = c(0.184, 0.330, 0.738, 0.576, -0.305, 0.390)
)
print(d)
require(Matrix)
X_sparse = with(d, sparseMatrix(i, j, x=x, dims=c(4,3)))
print(X_sparse)
Xs = matrix.sparse(X_sparse)
print(Xs)

## ----concat-------------------------------------------------------------------
Xds = matrix.concatenate(list(Xd, Xs))
print(Xds)
Xds$rows; Xds$cols
print(Xds)

## ----mixed--------------------------------------------------------------------
X_mixed = cbind(X_dense, c(1,0,2,1))
print(X_mixed)
levels = c(1,1,3)

## ----onehot-------------------------------------------------------------------
Xoh = matrix.one_hot(X_mixed, levels = levels)
Xoh$cols

## ----inoh---------------------------------------------------------------------
eye= as(as(diag(5), "generalMatrix"), "RsparseMatrix")
Xoh$sp_tmul(eye)

## ----std----------------------------------------------------------------------
Xohs = matrix.standardize(Xoh)
Xohs$sp_tmul(eye)

## ----attrs--------------------------------------------------------------------
print(attributes(Xohs))

## ----datasetup----------------------------------------------------------------
require(adelie)
n=1000
d_cont = 10     # number of continuous features
d_disc = 10     # number of discrete features
set.seed(3)     # random seed
Z_cont = matrix(rnorm(n*d_cont),n,d_cont)
levels = sample(2:10,d_disc,replace=TRUE)
Z_disc = matrix(0,n,d_disc)
for(i in seq(d_disc))Z_disc[,i] = sample(0:(levels[i]-1),n,replace=TRUE)

## ----standardize--------------------------------------------------------------
sd0 = function(x)sd(x)*sqrt(1-1/length(x)) # SD formula with divition by n rather n-1
Z_cont_means = apply(Z_cont,2,mean)
Z_cont_stds = apply(Z_cont,2,sd0)
Z_cont = scale(Z_cont, Z_cont_means,Z_cont_stds)

## ----comb---------------------------------------------------------------------
Z = cbind(Z_cont,Z_disc)
levels = c(rep(1,d_cont),levels)
print(levels)

## ----makey--------------------------------------------------------------------
xmat = model.matrix(~Z_cont[,1]*factor(Z_disc[,2]))
nc=ncol(xmat)
print(nc)
set.seed(4)
beta = rnorm(nc)
y = xmat%*%beta+rnorm(n)*2.5

## ----fig.width=7, fig.height=5------------------------------------------------
set.seed(2)
fit = glintnet(Z,glm.gaussian(y),levels=levels,intr_keys = 1)
cvfit = cv.glintnet(Z,glm.gaussian(y),levels=levels,intr_keys = 1)
par(mfrow=c(1,2))
plot(fit)
plot(cvfit)

## -----------------------------------------------------------------------------
predict(cvfit,type="nonzero")

## -----------------------------------------------------------------------------
coef(cvfit)

## ----fig.width=7, fig.height=5------------------------------------------------
set.seed(2)
fit2 = glintnet(Z,glm.gaussian(y),levels=levels)
cvfit2 = cv.glintnet(Z,glm.gaussian(y),levels=levels)
par(mfrow=c(1,2))
plot(fit2)
plot(cvfit2)

## -----------------------------------------------------------------------------
predict(cvfit2,type="nonzero")

## -----------------------------------------------------------------------------
predict(cvfit, newx=Z[1:3,])
predict(cvfit, newx=Z[1:3,],lambda="lambda.min")
print(cvfit)

## -----------------------------------------------------------------------------
print(fit)

## ----interact-----------------------------------------------------------------
X_int = matrix.interaction(Z,intr_keys = 1, intr_values=list(NULL),levels=levels)

## -----------------------------------------------------------------------------
pairs = attributes(X_int)[["_pairs"]] # base 0
pair_levels = apply(pairs,1,function(i)levels[i+1]) # it is transposed
is_cont_cont = apply(pair_levels,2,prod) == 1
cont_cont_pairs = pairs[is_cont_cont,]
cont_cont = Z[,cont_cont_pairs[,1]+1]*Z[,cont_cont_pairs[,2]+1] # base 0
centers = rep(0,X_int$cols)
scales = rep(1,X_int$cols)
cont_cont_indices = X_int$groups[is_cont_cont]+2 # base 0
centers[cont_cont_indices+1] = apply(cont_cont,2,mean) # base 0 index, so add 1
scales[cont_cont_indices+1] = apply(cont_cont,2,sd)  # base 0 index, so add 1

## -----------------------------------------------------------------------------
X_one_hot = matrix.one_hot(Z, levels)
X_int_std = matrix.standardize(
    X_int,
    centers=centers,
    scales=scales)
X = matrix.concatenate(
    list(
        X_one_hot,
        X_int_std)
    )
X$cols

## -----------------------------------------------------------------------------
groups = c(
    as.vector(X_one_hot$groups),
    X_one_hot$cols + as.vector(X_int$groups)) +1 # the +1 is the base 0 issue
is_cont_disc = apply(pair_levels-1,2,function(x)xor(x[1],x[2]))
penalty_int = rep(1, length(X_int$groups))
penalty_int[is_cont_cont] = sqrt(3)
penalty_int[is_cont_disc] = sqrt(2)
penalty = c(
    rep(1, length(X_one_hot$groups)),
    penalty_int)

## ----fig.width=7, fig.height=5------------------------------------------------
set.seed(2)
fit = grpnet(X,glm.gaussian(y),groups=groups,penalty=penalty, standardize=FALSE)
cv.fit = cv.grpnet(X,glm.gaussian(y),groups=groups,penalty=penalty, standardize=FALSE)
par(mfrow=c(1,2))
plot(fit)
plot(cv.fit)

