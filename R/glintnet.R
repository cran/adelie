#' fit a GLM interaction model with group lasso or group elastic-net regularization
#'
#' This function is an implementation of the \code{glinternet} model of Lim and Hastie, for fitting interactions between pairs of variables in a model. The method creates \emph{interaction matrices} and enforces hierarchy using the \emph{overlap group lasso}. Once the augmented model matrix is set up,
#' \code{glintnet} uses \code{grpnet} to fit the overlap group lasso path. It hence inherits all the capabilities of
#' \code{grpnet}, and in particular can fit interaction models for  all the GLM families.
#'
#' The input matrix can be composed of quantitative variables or columns representing factors.
#' The argument \code{levels} indicates which are quantitative, and which are factors.
#' The later are represented by numbers starting at 0, up to one less than the number of levels (sorry!)
#' Each of the factors are converted to "one-hot" matrices, and hence a group of columns are created for each of these.
#' This is done using the matrix utility function \code{matrix.one_hot()}. In addition interaction matrices are created.
#' For each pair of variables for which an interaction is considered, a matrix is created consisting of the
#' cross-product of each of the constituent matrices, as described in the "glinternet" reference.
#' Once this much bigger matrix is established, the model is handed to \code{grpnet} to produce the fit.
#'
#' @param X A dense matrix, which can include factors with levels coded as non-negative integers starting at 0.
#' @param glm GLM family/response object. This is an expression that
#'     represents the family, the reponse and other arguments such as
#'     weights, if present. The choices are \code{glm.gaussian()},
#'     \code{glm.binomial()}, \code{glm.poisson()},
#'     \code{glm.multinomial()}, \code{glm.cox()}, \code{glm.multinomial()},
#'     and \code{glm.multigaussian()}. This is a required argument, and
#'     there is no default. In the simple example below, we use \code{glm.gaussian(y)}.
#' @param offsets Offsets, default is \code{NULL}. If present, this is
#'     a fixed vector or matrix corresponding to the shape of the natural
#'     parameter, and is added to the fit.
#' @param   intr_keys   List of feature indices. This is a list of all features with which interactions can be
#' formed. Default is \code{1:p} where \code{p} is the number of columns in \code{X}.
#' @param   intr_values List of integer vectors of feature indices. For each of the \code{m <= p} indices
#' listed in \code{intr_keys}, there is a vector of indices indicating which columns are candidates for
#' interaction with that feature. If a vector is \code{NULL}, that means all other features are candidates
#' for interactions.  The default is a list of length \code{m} where each element is \code{NULL};
#' that is \code{rep(list(NULL), m}.
#' @param   levels Number of levels for each of the columns of \code{mat}, with \code{1} representing a
#' quantitative feature. A factor with \code{K} levels should be represented by the numbers \code{0,1,...,K-1}.
#' @param n_threads Number of threads, default \code{1}.
#' @param save.X Logical flag, default \code{FALSE}. If \code{TRUE}, the internally constructed X matrix is returned.
#' @param \dots Additional named arguments to \code{grpnet}.
#'
#' @return A list of class \code{"glintnet"}, which inherits from class \code{"grpnet"}.
#' This has a a few additional components such as \code{pairs}, \code{groups} and \code{levels}.
#' Users typically use methods like \code{predict()}, \code{print()}, \code{plot()} etc to examine the object.
#' @author James Yang, Trevor Hastie, and  Balasubramanian Narasimhan \cr Maintainer: Trevor Hastie
#' \email{hastie@@stanford.edu}
#'
#' @references
#' Lim, Michael and Hastie, Trevor (2015) \emph{Learning interactions via hierarchical group-lasso regularization}, JCGS
#' \doi{10.1080/10618600.2014.938812}\cr
#' Yang, James and Hastie, Trevor. (2024) A Fast and Scalable Pathwise-Solver for Group Lasso
#' and Elastic Net Penalized Regression via Block-Coordinate Descent. arXiv \doi{10.48550/arXiv.2405.08631}.\cr
#' Friedman, J., Hastie, T. and Tibshirani, R. (2008)
#' \emph{Regularization Paths for Generalized Linear Models via Coordinate
#' Descent (2010), Journal of Statistical Software, Vol. 33(1), 1-22},
#' \doi{10.18637/jss.v033.i01}.\cr
#' Simon, N., Friedman, J., Hastie, T. and Tibshirani, R. (2011)
#' \emph{Regularization Paths for Cox's Proportional
#' Hazards Model via Coordinate Descent, Journal of Statistical Software, Vol.
#' 39(5), 1-13},
#' \doi{10.18637/jss.v039.i05}.\cr
#' Tibshirani,Robert, Bien, J., Friedman, J., Hastie, T.,Simon, N.,Taylor, J. and
#' Tibshirani, Ryan. (2012) \emph{Strong Rules for Discarding Predictors in
#' Lasso-type Problems, JRSSB, Vol. 74(2), 245-266},
#' \url{https://arxiv.org/abs/1011.2234}.\cr

#' @seealso \code{cv.glintnet}, \code{predict.glintnet}, \code{plot.glintnet}, \code{print.glintnet}.
#' @examples
#' set.seed(0)
#' n=500
#' d_cont = 5     # number of continuous features
#' d_disc = 5     # number of categorical features
#' Z_cont = matrix(rnorm(n*d_cont), n, d_cont)
#' levels = sample(2:5,d_disc, replace = TRUE)
#' Z_disc = matrix(0,n,d_disc)
#' for(i in seq(d_disc))Z_disc[,i] = sample(0:(levels[i]-1),n,replace=TRUE)
#' Z = cbind(Z_cont,Z_disc)
#' levels = c(rep(1,d_cont),levels)
#'
#' xmat = model.matrix(~Z_cont[,1]*factor(Z_disc[,2]))
#' nc=ncol(xmat)
#' beta = rnorm(nc)
#' y = xmat%*%beta+rnorm(n)*1.5
#'
#' fit <- glintnet(Z, glm.gaussian(y), levels=levels, intr_keys = 1)
#' print(fit)
#'
#' @export
#' @importFrom stats sd


glintnet <- function(
    X,
    glm,
    offsets = NULL,
    intr_keys = NULL,
    intr_values,
    levels=NULL,
    n_threads = 1,
    save.X = FALSE,
    ...
)
{
    this.call <- match.call()
    d = ncol(X)
    if(is.null(levels))levels = rep(1,d)
    if(is.null(intr_keys))intr_keys = 1:d
    if(missing(intr_values))
        intr_values = rep(list(NULL),length(intr_keys))
    else{
        if(length(intr_values) != length(intr_keys))
            stop("the length of intr_values should be the same as length of intr_keys")
    }
    matob = matrix.glintnet(X,
                            intr_keys,
                            intr_values,
                            levels,
                            n_threads=n_threads,
                            stand = NULL
                            )

    fit = grpnet(matob$X,glm,
                 offsets=offsets,
                 groups=matob$groups,
                 penalty=matob$penalty,
                 standardize=FALSE,
                 n_threads=n_threads,
                 ...)
    fit$glint_stand = matob$stand
    fit=c(fit,list(intr_keys=intr_keys,intr_values=intr_values,levels=levels,pairs=matob$pairs+1))
    if(save.X)fit$X = matob$X
    class(fit)=c("glintnet","grpnet")
    fit$call=this.call
    fit
    }

matrix.glintnet <- function(
                            X,
                            intr_keys,
                            intr_values,
                            levels,
                            n_threads = 1,
                            stand=NULL
                            ){
    if(is.null(stand)){# need to compute from scratch
        fresh=TRUE
    }
    else fresh = FALSE
    if(fresh){
        d = ncol(X)
        d_disc = sum(levels>1)
        d_cont = sum(levels==1)
### Standardize the continuous features
        if(d_cont>0){
            which_cont = levels==1
            sd0 = function(x)sd(x)*sqrt(1-1/length(x)) # SD formula with division by n rather n-1
            X_cont_means = apply(X[,which_cont],2,mean)
            X_cont_stds = apply(X[,which_cont],2,sd0)
            stand$cont=list(cont_means=X_cont_means,
                            cont_stds=X_cont_stds,
                            which_cont=which_cont)
    }
    }
    if(!is.null(stand$cont)){
        wh = stand$cont$which_cont
        X[,wh] = scale(X[,wh], stand$cont$cont_means, stand$cont$cont_stds)
    }

### Make the interaction matrix
    X_int = matrix.interaction(X,
                               intr_keys = intr_keys,
                               intr_values=intr_values,
                               levels=levels)
### Grouping info we need later
    int_groups=X_int$groups
### Scale the cont_cont interactions
    if(fresh){
        pairs = attributes(X_int)[["_pairs"]] # base 0
        pair_levels = apply(pairs,1,function(i)levels[i+1]) # it is transposed
        is_cont_cont = apply(pair_levels,2,prod) == 1

        if(any(is_cont_cont)){
            cont_cont_pairs = pairs[is_cont_cont,]
            cont_cont = X[,cont_cont_pairs[,1]+1]*X[,cont_cont_pairs[,2]+1] # base 0
            centers = rep(0,X_int$cols)
            scales = rep(1,X_int$cols)
            cont_cont_indices = X_int$groups[is_cont_cont]+2 # base 0
            centers[cont_cont_indices+1] = apply(cont_cont,2,mean) # base 0 index, so add 1
            scales[cont_cont_indices+1] = apply(cont_cont,2,sd)  # base 0 index, so add 1
            stand$cont_cont = list(centers=centers,scales=scales)
        }
    }
    if(!is.null(stand$cont_cont)){
        X_int= matrix.standardize(
            X_int,
            centers=stand$cont_cont$centers,
            scales=stand$cont_cont$scales)
    }
### make the big matrix
    X_one_hot = matrix.one_hot(X, levels)
    X_big = matrix.concatenate(
        list(
            X_one_hot,
            X_int)
    )
    if(fresh){
### Prepare the grouping and penalty factor
        groups = c(
            as.vector(X_one_hot$groups),
            X_one_hot$cols + as.vector(int_groups)) +1 # the +1 is the base 0 issue
        is_cont_disc = apply(pair_levels-1,2,function(x)xor(x[1],x[2]))
        penalty_int = rep(1, length(int_groups))
        penalty_int[is_cont_cont] = sqrt(3)
        penalty_int[is_cont_disc] = sqrt(2)
        penalty = c(
            rep(1, length(X_one_hot$groups)),
            penalty_int)
### Return a list including X
        list(X=X_big,groups=groups,levels=levels,pairs=pairs, penalty=penalty,stand=stand)
    } else X_big
}

#' make predictions from a "glintnet" object.
#'
#' Similar to other predict methods, this functions predicts linear predictors,
#' coefficients and more from a fitted \code{"glintnet"} object.
#' @inherit predict.grpnet params
#' @aliases coef.glintnet
#' @param object Fitted \code{"glintnet"} model.
#' @param newx Matrix of new values for \code{x} at which predictions are to be
#' made. This matrix is of the same form as in the call to \code{glintnet}.
#' @param \dots Other arguments that can be passed to \code{predict.grpnet}
#' @return The object returned depends on type.
#' @author James Yang, Trevor Hastie, and  Balasubramanian Narasimhan \cr Maintainer: Trevor Hastie
#' \email{hastie@stanford.edu}
#' @seealso \code{grpnet}, and \code{print}, and \code{coef} methods, and
#' \code{cv.grpnet}.
#' @references Yang, James and Hastie, Trevor. (2024) A Fast and Scalable Pathwise-Solver for Group Lasso
#' and Elastic Net Penalized Regression via Block-Coordinate Descent. arXiv \doi{10.48550/arXiv.2405.08631}.\cr
#' Adelie Python user guide  \url{https://jamesyang007.github.io/adelie/}
#' @keywords models regression
#'
#' @examples
#' set.seed(0)
#' n=500
#' d_cont = 5     # number of continuous features
#' d_disc = 5     # number of categorical features
#' Z_cont = matrix(rnorm(n*d_cont), n, d_cont)
#' levels = sample(2:5,d_disc, replace = TRUE)
#' Z_disc = matrix(0,n,d_disc)
#' for(i in seq(d_disc))Z_disc[,i] = sample(0:(levels[i]-1),n,replace=TRUE)
#' Z = cbind(Z_cont,Z_disc)
#' levels = c(rep(1,d_cont),levels)
#'
#' xmat = model.matrix(~Z_cont[,1]*factor(Z_disc[,2]))
#' nc=ncol(xmat)
#' beta = rnorm(nc)
#' y = xmat%*%beta+rnorm(n)*1.5
#'
#' fit <- glintnet(Z, glm.gaussian(y), levels=levels, intr_keys = 1)
#' predict(fit, lambda = c(.1,.01), newx = Z[1:4,])
#' predict(fit, lambda = c(0.1,0.01), type="nonzero")
#'
#' @method predict glintnet
#' @export
#' @export predict.glintnet

predict.glintnet <- function (object, newx, lambda = NULL, type = c("link", "response",
    "coefficients","nonzero"), newoffsets = NULL, n_threads = 1, ...)
{
    type = match.arg(type)
    if (!match(type, c("coefficients","nonzero"), FALSE)){
        if (missing(newx)) stop("You need to supply a value for 'newx'")
        matob = matrix.glintnet(
            newx,
            object$intr_keys,
            object$intr_values,
            object$levels,
            n_threads=n_threads,
            stand=object$glint_stand)
        return(predict.grpnet(object,newx=matob,lambda=lambda,type=type,
                              newoffsets=newoffsets,
                              n_threads=n_threads,
                              ...))
    }
    else{
        coefob = predict.grpnet(object,lambda=lambda,type="coefficients")
        if(type == "nonzero")
            nonzeroTerms(coefob,object$groups,object$levels,object$pairs)
        else
            coefob
        }
}

#' @method coef glintnet
#' @export
#' @export coef.glintnet

coef.glintnet <- function(object,lambda=NULL,...)predict(object, lambda = lambda, type = "coefficients", ...)

#' Cross-validation for glintnet
#'
#' Does k-fold cross-validation for glintnet
#'
#' The function runs \code{glintnet} \code{n_folds}+1 times; the first to get the
#' \code{lambda} sequence, and then the remainder to compute the fit with each
#' of the folds omitted. The out-of-fold deviance is accumulated, and the average deviance and
#' standard deviation over the folds is computed.  Note that \code{cv.glintnet}
#' does NOT search for values for \code{alpha}. A specific value should be
#' supplied, else \code{alpha=1} is assumed by default. If users would like to
#' cross-validate \code{alpha} as well, they should call \code{cv.glintnet} with
#' a pre-computed vector \code{foldid}, and then use this same \code{foldid} vector in
#' separate calls to \code{cv.glintnet} with different values of \code{alpha}.
#' Note also that the results of \code{cv.glintnet} are random, since the folds
#' are selected at random. Users can reduce this randomness by running
#' \code{cv.glintnet} many times, and averaging the error curves.
#'
#' @inherit glintnet
#' @inherit cv.grpnet params
#' @param X Feature matrix. Either a regular R matrix, or else an
#'     \code{adelie} custom matrix class, or a concatination of such.
#' @examples
#' set.seed(0)
#' n=500
#' d_cont = 5     # number of continuous features
#' d_disc = 5     # number of categorical features
#' Z_cont = matrix(rnorm(n*d_cont), n, d_cont)
#' levels = sample(2:5,d_disc, replace = TRUE)
#' Z_disc = matrix(0,n,d_disc)
#' for(i in seq(d_disc))Z_disc[,i] = sample(0:(levels[i]-1),n,replace=TRUE)
#' Z = cbind(Z_cont,Z_disc)
#' levels = c(rep(1,d_cont),levels)
#'
#' xmat = model.matrix(~Z_cont[,1]*factor(Z_disc[,2]))
#' nc=ncol(xmat)
#' beta = rnorm(nc)
#' y = xmat%*%beta+rnorm(n)*1.5
#'
#' cvfit <- cv.glintnet(Z, glm.gaussian(y), levels=levels, intr_keys = 1)
#' plot(cvfit)
#' predict(cvfit, newx=Z[1:5,])
#'

#' @export cv.glintnet
cv.glintnet <- function(
    X,
    glm,
    offsets = NULL,
    intr_keys = NULL,
    intr_values,
    levels=NULL,
    n_folds = 10,
    foldid = NULL,
    n_threads = 1,
    ...
    ){
    cv.call = match.call()
    d = ncol(X)
    if(is.null(levels))levels = rep(1,d)
   if(is.null(intr_keys))intr_keys = 1:d
    if(missing(intr_values))
        intr_values = rep(list(NULL),length(intr_keys))
    else{
        if(length(intr_values) != length(intr_keys))
            stop("the length of intr_values should be the same as length of intr_keys")
    }
   matob = matrix.glintnet(X,
                            intr_keys,
                            intr_values,
                            levels,
                            n_threads=n_threads,
                            stand = NULL
                            )
    out = cv.grpnet(matob$X,glm,
                 offsets=offsets,
                 groups=matob$groups,
                 penalty=matob$penalty,
                 standardize=FALSE,
                 n_folds=n_folds,
                 foldid=foldid,
                 n_threads=n_threads,
                 ...)
    obj = list(
        glint_stand=matob$stand,
        levels=levels,
        intr_keys=intr_keys,
        intr_values=intr_values,
        groups=matob$groups,
        pairs=matob$pairs+1
    )
    obj = c(obj, out$grpnet.fit)
    class(obj)=c("glintnet","grpnet")
    out$glintnet.fit = obj
    out$grpnet.fit=NULL
    out$call = cv.call
    class(out)=c("cv.glintnet","cv.grpnet")
    out
    }

#' make predictions from a "cv.glintnet" object.
#'
#' This function makes predictions from a cross-validated \code{glintnet} model, using
#' the stored \code{"glintnet.fit"} object, and the optimal value chosen for
#' \code{lambda}.
#'
#' This function makes it easier to use the results of cross-validation to make
#' a prediction.
#' @inherit predict.glintnet params
#' @inherit predict.cv.grpnet params
#' @aliases coef.cv.glintnet predict.cv.glintnet
#' @param object Fitted \code{"cv.glintnet"}.
#' @examples
#' set.seed(0)
#' n=500
#' d_cont = 5     # number of continuous features
#' d_disc = 5     # number of categorical features
#' Z_cont = matrix(rnorm(n*d_cont), n, d_cont)
#' levels = sample(2:5,d_disc, replace = TRUE)
#' Z_disc = matrix(0,n,d_disc)
#' for(i in seq(d_disc))Z_disc[,i] = sample(0:(levels[i]-1),n,replace=TRUE)
#' Z = cbind(Z_cont,Z_disc)
#' levels = c(rep(1,d_cont),levels)
#'
#' xmat = model.matrix(~Z_cont[,1]*factor(Z_disc[,2]))
#' nc=ncol(xmat)
#' beta = rnorm(nc)
#' y = xmat%*%beta+rnorm(n)*1.5
#'
#' cvfit <- cv.glintnet(Z, glm.gaussian(y), levels=levels, intr_keys = 1)
#' plot(cvfit)
#' predict(cvfit, newx=Z[1:5,])
#'
#' @method predict cv.glintnet
#' @export

predict.cv.glintnet <-function (object, newx, lambda = c("lambda.1se", "lambda.min"),
    ...)
{
    if (is.character(lambda)) {
        lambda = match.arg(lambda)
        namel = lambda
        lambda = object[[lambda]]
        names(lambda) = namel
    }
    predict(object$glintnet.fit, newx, lambda = lambda, ...)
}
#' @method coef cv.glintnet
#' @export
coef.cv.glintnet <-function (object, lambda = c("lambda.1se", "lambda.min"), ...)
{
    if (is.character(lambda)) {
        lambda = match.arg(lambda)
        lambda = object[[lambda]]
    }
    coef(object$glintnet.fit, lambda = lambda, ...)
}


#'
#' Print a summary of the glintnet path at each step along the path.
#' @details
#' The call that produced the object `x` is printed, followed by a
#' five-column matrix with columns `N_main`,  `N_int`, `Df`, `%Dev` and `Lambda`.
#' The `N_main` column is the number of main-effect terms in the solution, and `N_int` the number of interaction terms. Since an interaction term implies both main effects, the former is always at least as large as the latter.
#' The `Df` column is the number of nonzero coefficients (Df is a
#' reasonable name only for lasso fits). `%Dev` is the percent deviance
#' explained (relative to the null deviance).
#' @param x fitted glintnet object
#' @inherit print.grpnet
#'
#' @method print glintnet
#' @export

#'
print.glintnet <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall: ", deparse(x$call), "\n\n")
    coefstuff <- coef(x)
    groups = nonzeroGroup(
        coefstuff,
        x$groups,
        logical = TRUE
    )
    nmain=length(x$levels)
    groupid = seq_len(nmain)
    mainact = groups[groupid,]
    pairact = groups[-groupid,]
    N_int=apply(pairact,2,sum)
    mainpair = function(wh,pairs,nmain){
        main = logical(nmain)
        if(any(wh)){
            idm = unique(pairs[wh,])
            main[idm]=TRUE
        }
        main
    }
    pairs=x$pairs
    pairvars = apply(pairact,2,mainpair, pairs=pairs,nmain=nmain)
    mainact=mainact|pairvars
    N_main = apply(mainact,2,sum)
    Df = coefstuff$df
    dev.ratio = x$state$devs
    lambdas=coefstuff$lambda
    out = data.frame(N_main, N_int, Df, `%Dev` = round(dev.ratio *
        100, 2), Lambda = signif(lambdas, digits), check.names = FALSE,
        row.names = seq(along.with = Df))
    class(out) = c("anova", class(out))
    print(out)
    invisible(out)
}

#' plot the cross-validation curve produced by cv.glintnet
#'
#' Plots the cross-validation curve, and upper and lower standard deviation
#' curves, as a function of the \code{lambda} values used.
#'
#' A plot is produced, and nothing is returned.
#'
#' @rdname plot.cv.grpnet
#' @param x fitted \code{"cv.glintnet"} object
#' @examples
#' set.seed(0)
#' n=500
#' d_cont = 5     # number of continuous features
#' d_disc = 5     # number of categorical features
#' Z_cont = matrix(rnorm(n*d_cont), n, d_cont)
#' levels = sample(2:5,d_disc, replace = TRUE)
#' Z_disc = matrix(0,n,d_disc)
#' for(i in seq(d_disc))Z_disc[,i] = sample(0:(levels[i]-1),n,replace=TRUE)
#' Z = cbind(Z_cont,Z_disc)
#' levels = c(rep(1,d_cont),levels)
#'
#' xmat = model.matrix(~Z_cont[,1]*factor(Z_disc[,2]))
#' nc=ncol(xmat)
#' beta = rnorm(nc)
#' y = xmat%*%beta+rnorm(n)*1.5
#'
#' cvfit <- cv.glintnet(Z, glm.gaussian(y), levels=levels, intr_keys = 1)
#' plot(cvfit)
#'
#' @method plot cv.glintnet
#' @export
plot.cv.glintnet <- function(x, sign.lambda = -1, ...){
    x$grpnet.fit = x$glintnet.fit["family"]
    NextMethod("plot")
    }

### Utility function
nonzeroTerms <- function(coefob,group, levels, pairs){
    groups = nonzeroGroup(
        coefob,
        group,
        logical = TRUE
    )
    nmain=length(levels)
    groupid = seq_len(nmain)
    termf = function(group_act,groupid,pairs){
        main = group_act[groupid]
        int = group_act[-groupid]
         list(
            main = if(any(main)) groupid[main] else NULL,
            int = if(any(int))pairs[int,] else NULL
        )
}
    apply(groups,2,termf,groupid=groupid,pairs=pairs)
}
