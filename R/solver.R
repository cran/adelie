solve_ <- function(
    state, 
    progress_bar=FALSE
)
{
    # mapping of each state type to the corresponding solver
    f_dict <- list(
        # cov methods
        #"Rcpp_RStateGaussianPinCov64"=r_solve_gaussian_pin_cov_64,
        "Rcpp_RStateGaussianCov64"=r_solve_gaussian_cov_64,
        # naive methods
        #"Rcpp_RStateGaussianPinNaive64"=r_solve_gaussian_pin_naive_64,
        "Rcpp_RStateGaussianNaive64"=r_solve_gaussian_naive_64,
        "Rcpp_RStateMultiGaussianNaive64"=r_solve_multigaussian_naive_64,
        "Rcpp_RStateGlmNaive64"=r_solve_glm_naive_64,
        "Rcpp_RStateMultiGlmNaive64"=r_solve_multiglm_naive_64
    )

    is_gaussian_pin <- (
        (class(state) == "Rcpp_RStateGaussianPinCov64") ||
        (class(state) == "Rcpp_RStateGaussianPinNaive64")
    )
    is_gaussian <- (
        (class(state) == "Rcpp_RStateGaussianCov64") ||
        (class(state) == "Rcpp_RStateGaussianNaive64") ||
        (class(state) == "Rcpp_RStateMultiGaussianNaive64")
    )
    is_glm <- (
        (class(state) == "Rcpp_RStateGlmNaive64") ||
        (class(state) == "Rcpp_RStateMultiGlmNaive64")
    )

    # solve group elastic net
    f <- f_dict[[class(state)[1]]]
    if (is_gaussian_pin) {
        out <- f(state)
    } else if (is_gaussian) {
        out <- f(state, progress_bar)
    } else if (is_glm) {
        out <- f(state, attr(state, "_glm"), progress_bar)
    } else {
        stop("Unexpected state type.")
    }

    # raise any errors
    if (out[["error"]] != "") {
        warning(out[["error"]])
    }

    # return a subsetted result object
    core_state <- out[["state"]]
    state <- state.create_from_core(state, core_state)

    # add extra total time information
    attr(state, "total_time") <- out[["total_time"]]

    state
}

#' Solves group elastic net via covariance method.
#' 
#' @param   A   Positive semi-definite matrix.
#' @param   v   Linear term.
#' @param   constraints     Constraints.
#' @param   groups  Groups.
#' @param   alpha   Elastic net parameter.
#' @param   penalty Penalty factor.
#' @param   lmda_path   The regularization path.
#' @param   max_iters   Maximum number of coordinate descents.
#' @param   tol     Coordinate descent convergence tolerance.
#' @param   rdev_tol    Relative percent deviance explained tolerance.
#' @param   newton_tol  Convergence tolerance for the BCD update.
#' @param   newton_max_iters    Maximum number of iterations for the BCD update.
#' @param   n_threads   Number of threads.
#' @param   early_exit  \code{TRUE} if the function should early exit.
#' @param   screen_rule Screen rule.
#' @param   min_ratio   Ratio between largest and smallest regularization.
#' @param   lmda_path_size Number of regularizations.
#' @param   max_screen_size Maximum number of screen groups.
#' @param   max_active_size Maximum number of active groups.
#' @param   pivot_subset_ratio  Subset ratio of pivot rule.
#' @param   pivot_subset_min    Minimum subset of pivot rule.
#' @param   pivot_slack_ratio   Slack ratio of pivot rule.
#' @param   check_state     Check state.
#' @param   progress_bar    Progress bar.
#' @param   warm_start      Warm start.
#' @returns State of the solver.
#' 
#' @examples
#' set.seed(0)
#' n <- 100
#' p <- 200
#' X <- matrix(rnorm(n * p), n, p)
#' y <- X[,1] * rnorm(1) + rnorm(n)
#' A <- t(X) %*% X / n
#' v <- t(X) %*% y / n
#' state <- gaussian_cov(A, v)
#' 
#' @export
gaussian_cov <- function(
    A,
    v,
    constraints = NULL,
    groups = NULL,
    alpha = 1,
    penalty = NULL,
    lmda_path = NULL,
    max_iters = as.integer(1e5),
    tol = 1e-7,
    rdev_tol=1e-3,
    newton_tol = 1e-12,
    newton_max_iters = 1000,
    n_threads = 1,
    early_exit = TRUE,
    screen_rule = "pivot",
    min_ratio = 1e-2,
    lmda_path_size = 100,
    max_screen_size = NULL,
    max_active_size = NULL,
    pivot_subset_ratio = 0.1,
    pivot_subset_min = 1,
    pivot_slack_ratio = 1.25,
    check_state = FALSE,
    progress_bar = TRUE,
    warm_start = NULL
)
{
    if (is.matrix(A) || is.array(A) || is.data.frame(A)) {
        A <- matrix.dense(A, method="cov", n_threads=n_threads)
    }

    p <- A$cols

    if (!is.null(lmda_path)) {
        lmda_path <- sort(lmda_path, decreasing=TRUE)
    } 

    if (is.null(groups)) {
        groups <- as.integer(0:(p-1))
    } else {
        groups <- as.integer(groups)
    }
    group_sizes <- c(groups, p)
    group_sizes <- group_sizes[2:length(group_sizes)] - group_sizes[1:(length(group_sizes)-1)]
    group_sizes <- as.integer(group_sizes)
    G <- length(groups)

    if (is.null(penalty)) {
        penalty <- sqrt(group_sizes)
    } else {
        penalty <- as.double(penalty)
    }

    if (is.null(warm_start)) {
        lmda <- Inf
        lmda_max <- NULL
        screen_set <- (0:(G-1))[(penalty <= 0) | (alpha <= 0)]
        screen_beta <- double(sum(group_sizes[screen_set + 1]))
        screen_is_active <- as.integer(rep_len(1, length(screen_set)))
        screen_dual_size <- 0
        if (!is.null(constraints)) {
            screen_dual_size <- as.integer(sum(
                sapply(screen_set, function(k) {
                    ifelse(is.null(constraints[k+1]), 0, constraints[k+1]$dual_size)
                })
            ))
        }
        screen_dual <- double(screen_dual_size)
        active_set_size <- length(screen_set)
        active_set <- integer(G)
        if (active_set_size > 0) {
            active_set[1:active_set_size] <- 0:(active_set_size-1)
        }
        rsq <- 0

        subset <- c()
        for (ss in screen_set) {
            subset <- c(subset, groups[ss+1]:(groups[ss+1]+group_sizes[ss+1]-1))
        }
        subset <- as.integer(subset)
        order <- order(subset)
        indices <- as.integer(subset[order])
        values <- as.double(screen_beta[order])

        grad <- v - A$mul(indices, values)
    } else {
        lmda <- as.double(warm_start$lmda)
        lmda_max <- as.double(warm_start$lmda_max)
        screen_set <- as.integer(warm_start$screen_set)
        screen_beta <- as.double(warm_start$screen_beta)
        screen_is_active <- as.integer(warm_start$screen_is_active)
        screen_dual <- as.double(warm_start$screen_dual)
        active_set_size <- as.integer(warm_start$active_set_size)
        active_set <- as.integer(warm_start$active_set)
        rsq <- as.double(warm_start$rsq)
        grad <- as.double(warm_start$grad)
    }

    state <- state.gaussian_cov(
        A=A,
        v=v,
        constraints=constraints,
        groups=groups,
        group_sizes=group_sizes,
        alpha=alpha,
        penalty=penalty,
        screen_set=screen_set,
        screen_beta=screen_beta,
        screen_is_active=screen_is_active,
        screen_dual=screen_dual,
        active_set_size=active_set_size,
        active_set=active_set,
        rsq=rsq,
        lmda=lmda,
        grad=grad,
        lmda_path=lmda_path,
        lmda_max=lmda_max,
        max_iters=max_iters,
        tol=tol,
        rdev_tol=rdev_tol,
        newton_tol=newton_tol,
        newton_max_iters=newton_max_iters,
        n_threads=n_threads,
        early_exit=early_exit,
        screen_rule=screen_rule,
        min_ratio=min_ratio,
        lmda_path_size=lmda_path_size,
        max_screen_size=max_screen_size,
        max_active_size=max_active_size,
        pivot_subset_ratio=pivot_subset_ratio,
        pivot_subset_min=pivot_subset_min,
        pivot_slack_ratio=pivot_slack_ratio
    )

    solve_(
        state=state,
        progress_bar=progress_bar
    )
}

#' Solves group elastic net via naive method.
#' 
#' @param   X   Feature matrix.
#' @param   glm     GLM object.
#' @param   constraints Constraints.
#' @param   groups  Groups.
#' @param   alpha   Elastic net parameter.
#' @param   penalty Penalty factor.
#' @param   offsets Offsets.
#' @param   lmda_path   The regularization path.
#' @param   irls_max_iters  Maximum number of IRLS iterations.
#' @param   irls_tol  IRLS convergence tolerance.
#' @param   max_iters   Maximum number of coordinate descents.
#' @param   tol     Coordinate descent convergence tolerance.
#' @param   adev_tol    Percent deviance explained tolerance.
#' @param   ddev_tol    Difference in percent deviance explained tolerance.
#' @param   newton_tol  Convergence tolerance for the BCD update.
#' @param   newton_max_iters    Maximum number of iterations for the BCD update.
#' @param   n_threads   Number of threads.
#' @param   early_exit  \code{TRUE} if the function should early exit.
#' @param   intercept   \code{TRUE} to fit with intercept.
#' @param   screen_rule Screen rule.
#' @param   min_ratio   Ratio between largest and smallest regularization.
#' @param   lmda_path_size Number of regularizations.
#' @param   max_screen_size Maximum number of screen groups.
#' @param   max_active_size Maximum number of active groups.
#' @param   pivot_subset_ratio  Subset ratio of pivot rule.
#' @param   pivot_subset_min    Minimum subset of pivot rule.
#' @param   pivot_slack_ratio   Slack ratio of pivot rule.
#' @param   check_state     Check state.
#' @param   progress_bar    Progress bar.
#' @param   warm_start      Warm start.
#' @returns State of the solver.
#'
#' @examples
#' set.seed(0)
#' n <- 100
#' p <- 200
#' X <- matrix(rnorm(n * p), n, p)
#' y <- X[,1] * rnorm(1) + rnorm(n)
#' state <- grpnet(X, glm.gaussian(y))
#'
#' @export
grpnet <- function(
    X,
    glm,
    constraints = NULL,
    groups = NULL,
    alpha = 1,
    penalty = NULL,
    offsets = NULL,
    lmda_path = NULL,
    irls_max_iters = as.integer(1e4),
    irls_tol = 1e-7,
    max_iters = as.integer(1e5),
    tol = 1e-7,
    adev_tol = 0.9,
    ddev_tol = 0,
    newton_tol = 1e-12,
    newton_max_iters = 1000,
    n_threads = 1,
    early_exit = TRUE,
    intercept = TRUE,
    screen_rule = "pivot",
    min_ratio = 1e-2,
    lmda_path_size = 100,
    max_screen_size = NULL,
    max_active_size = NULL,
    pivot_subset_ratio = 0.1,
    pivot_subset_min = 1,
    pivot_slack_ratio = 1.25,
    check_state = FALSE,
    progress_bar = TRUE,
    warm_start = NULL
)
{
    X_raw <- X

    if (is.matrix(X) || is.array(X) || is.data.frame(X)) {
        X <- matrix.dense(X, method="naive", n_threads=n_threads)
    }

    n <- X$rows
    p <- X$cols

    y <- glm$y 
    weights <- as.double(glm$weights)
    if (is.null(dim(y))) {
        y <- as.double(y)
    }

    # compute common quantities
    if (!is.null(offsets)) {
        offsets <- as.double(offsets)
        if ((dim(offsets) != dim(y)) ||
            (length(offsets) != length(y))
        ) {
            stop("offsets must be same shape as y if not NULL.")
        }
    } else {
        # 1-dimensional
        if (is.null(dim(y))) {
            offsets <- double(length(y))
        # 2-dimensional
        } else {
            offsets <- matrix(0.0, nrow(y), ncol(y))
        }
    }

    if (!is.null(lmda_path)) {
        lmda_path <- sort(lmda_path, decreasing=TRUE)
    } 

    solver_args <- list(
        X=X_raw,
        constraints=constraints,
        alpha=alpha,
        offsets=offsets,
        lmda_path=lmda_path,
        max_iters=max_iters,
        tol=tol,
        adev_tol=adev_tol,
        ddev_tol=ddev_tol,
        newton_tol=newton_tol,
        newton_max_iters=newton_max_iters,
        n_threads=n_threads,
        early_exit=early_exit,
        intercept=intercept,
        screen_rule=screen_rule,
        min_ratio=min_ratio,
        lmda_path_size=lmda_path_size,
        max_screen_size=max_screen_size,
        max_active_size=max_active_size,
        pivot_subset_ratio=pivot_subset_ratio,
        pivot_subset_min=pivot_subset_min,
        pivot_slack_ratio=pivot_slack_ratio
    )

    # do special routine for optimized gaussian
    is_gaussian_opt <- (
        (glm$name %in% list("gaussian", "multigaussian")) &&
        attr(glm, "opt")
    )

    # add a few more configs in GLM case
    if (!is_gaussian_opt) {
        solver_args[["glm"]] <- glm
        solver_args[["irls_max_iters"]] <- irls_max_iters
        solver_args[["irls_tol"]] <- irls_tol
    } else {
        solver_args[["y"]] <- y
        solver_args[["weights"]] <- weights
    }

    # multi-response GLMs
    if (glm$is_multi) {
        K <- ncol(y)

        if (is.null(groups)) {
            groups <- "grouped"
        } 
        if (groups == "grouped") {
            groups <- as.integer(K * ((1:p) - 1))
        } else if (groups == "ungrouped") {
            groups <- as.integer((1:(K*p)) - 1)
        } else {
            stop("groups must be one of \"grouped\" or \"ungrouped\" for multi-response.")
        }

        if (intercept) {
            groups <- as.integer(c((1:K)-1, K + groups))
        }
        group_sizes <- as.integer(c(groups, (p+intercept) * K))
        group_sizes <- as.integer(
            group_sizes[2:length(group_sizes)] - group_sizes[1:(length(group_sizes)-1)]
        )

        if (is.null(penalty)) {
            penalty <- sqrt(group_sizes)
            if (intercept) {
                penalty[1:K] = 0
            }
        } else {
            if (intercept) {
                penalty <- c(double(K), penalty)
            }
        }

        G <- length(groups)

        if (is.null(warm_start)) {
            lmda <- Inf
            lmda_max <- NULL
            screen_set <- (0:(G-1))[(penalty <= 0) | (alpha <= 0)]
            screen_beta <- double(sum(group_sizes[screen_set + 1]))
            screen_is_active <- as.integer(rep_len(1, length(screen_set)))
            screen_dual_size <- 0
            if (!is.null(constraints)) {
                screen_dual_size <- as.integer(sum(
                    sapply(screen_set, function(k) {
                        ifelse(is.null(constraints[k+1]), 0, constraints[k+1]$dual_size)
                    })
                ))
            }
            screen_dual <- double(screen_dual_size)
            active_set_size <- length(screen_set)
            active_set <- integer(G)
            if (active_set_size > 0) {
                active_set[1:active_set_size] <- 0:(active_set_size-1)
            }
        } else {
            lmda <- as.double(warm_start$lmda)
            lmda_max <- as.double(warm_start$lmda_max)
            screen_set <- as.integer(warm_start$screen_set)
            screen_beta <- as.double(warm_start$screen_beta)
            screen_is_active <- as.integer(warm_start$screen_is_active)
            screen_dual <- as.double(warm_start$screen_dual)
            active_set_size <- as.integer(warm_start$active_set_size)
            active_set <- as.integer(warm_start$active_set)
        }

        solver_args[["groups"]] <- groups
        solver_args[["group_sizes"]] <- group_sizes
        solver_args[["penalty"]] <- penalty
        solver_args[["lmda"]] <- lmda
        solver_args[["lmda_max"]] <- lmda_max
        solver_args[["screen_set"]] <- screen_set
        solver_args[["screen_beta"]] <- screen_beta
        solver_args[["screen_is_active"]] <- screen_is_active
        solver_args[["screen_dual"]] <- screen_dual
        solver_args[["active_set_size"]] <- active_set_size
        solver_args[["active_set"]] <- active_set

        # represent the augmented X matrix as used in single-response reformatted problem.
        X_aug <- matrix.kronecker_eye(X_raw, K, n_threads=n_threads)
        if (intercept) {
            X_aug <- matrix.concatenate(list(
                matrix.kronecker_eye(
                    matrix(rep_len(1.0, n), n, 1), K, n_threads=n_threads
                ),
                X_aug
            ), axis=1, n_threads=n_threads)
        }

        if (is_gaussian_opt) {
            weights_mscaled <- weights / K
            if (is.null(warm_start)) {
                ones <- rep(1.0, n)
                X_means <- X$mul(ones, weights_mscaled)
                X_means <- rep(X_means, each=K)
                if (intercept) {
                    X_means <- c(rep_len(1/K, K), X_means)
                }
                y_off <- y - offsets
                # variance of y that gaussian solver expects
                y_var <- sum(weights_mscaled * y_off ** 2)
                # variance for the null model with multi-intercept
                # R^2 can be initialized to MSE under intercept-model minus y_var.
                # This is a negative quantity in general, but will be corrected to 0
                # when the model fits the unpenalized (including intercept) term.
                # Then, supplying y_var as the normalization will result in R^2 
                # relative to the intercept-model.
                if (intercept) {
                    y_off_c <- t(t(y_off) - as.double(t(y_off) %*% weights)) # NOT a typo: weights
                    yc_var <- sum(weights_mscaled * y_off_c ** 2)
                    rsq <- yc_var - y_var
                    y_var <- yc_var
                } else {
                    rsq <- 0.0
                }
                resid <- as.double(t(y_off))
                resid_sum <- sum(weights_mscaled * y_off)
                weights_mscaled <- rep(weights_mscaled, each=K)
                grad <- X_aug$mul(resid, weights_mscaled)
            } else {
                X_means <- as.double(warm_start$X_means)
                y_var <- as.double(warm_start$y_var)
                rsq <- as.double(warm_start$rsq)
                resid <- as.double(warm_start$resid)
                resid_sum <- as.double(warm_start$resid_sum)
                grad <- as.double(warm_start$grad)
            }

            solver_args[["X_means"]] <- X_means
            solver_args[["y_var"]] <- y_var
            solver_args[["rsq"]] <- rsq
            solver_args[["resid"]] <- resid
            solver_args[["resid_sum"]] <- resid_sum
            solver_args[["grad"]] <- grad

            state <- do.call(state.multigaussian_naive, solver_args)
        
        # GLM case
        } else {
            if (is.null(warm_start)) {
                ones <- rep_len(1.0, length(offsets))
                eta <- offsets
                etaT <- t(eta)
                residT <- glm$gradient(etaT)
                resid <- as.double(residT)
                grad <- X_aug$mul(resid, ones)
                loss_null <- NULL
                loss_full <- as.double(glm$loss_full())
            } else {
                eta <- as.double(warm_start$eta)
                resid <- as.double(warm_start$resid)
                grad <- as.double(warm_start$grad)
                loss_null <- as.double(warm_start$loss_null)
                loss_full <- as.double(warm_start$loss_full)
            }

            solver_args[["grad"]] <- grad
            solver_args[["eta"]] <- eta
            solver_args[["resid"]] <- resid
            solver_args[["loss_null"]] <- loss_null
            solver_args[["loss_full"]] <- loss_full

            state <- do.call(state.multiglm_naive, solver_args)
        }

    # single-response GLMs
    } else {
        if (is.null(groups)) {
            groups <- as.integer(0:(p-1))
        } else {
            groups <- as.integer(groups)
        }
        group_sizes <- c(groups, p)
        group_sizes <- group_sizes[2:length(group_sizes)] - group_sizes[1:(length(group_sizes)-1)]
        group_sizes <- as.integer(group_sizes)

        G <- length(groups)

        if (is.null(penalty)) {
            penalty <- sqrt(group_sizes)
        } else {
            penalty <- as.double(penalty)
        }

        if (is.null(warm_start)) {
            lmda <- Inf
            lmda_max <- NULL
            screen_set <- (0:(G-1))[(penalty <= 0) | (alpha <= 0)]
            screen_beta <- double(sum(group_sizes[screen_set + 1]))
            screen_is_active <- as.integer(rep_len(1, length(screen_set)))
            screen_dual_size <- 0
            if (!is.null(constraints)) {
                screen_dual_size <- as.integer(sum(
                    sapply(screen_set, function(k) {
                        ifelse(is.null(constraints[k+1]), 0, constraints[k+1]$dual_size)
                    })
                ))
            }
            screen_dual <- double(screen_dual_size)
            active_set_size <- length(screen_set)
            active_set <- integer(G)
            if (active_set_size > 0) {
                active_set[1:active_set_size] <- 0:(active_set_size-1)
            }
        } else {
            lmda <- as.double(warm_start$lmda)
            lmda_max <- as.double(warm_start$lmda_max)
            screen_set <- as.integer(warm_start$screen_set)
            screen_beta <- as.double(warm_start$screen_beta)
            screen_is_active <- as.integer(warm_start$screen_is_active)
            screen_dual <- as.double(warm_start$screen_dual)
            active_set_size <- as.integer(warm_start$active_set_size)
            active_set <- as.integer(warm_start$active_set)
        }

        solver_args[["groups"]] <- groups
        solver_args[["group_sizes"]] <- group_sizes
        solver_args[["penalty"]] <- penalty
        solver_args[["lmda"]] <- lmda
        solver_args[["lmda_max"]] <- lmda_max
        solver_args[["screen_set"]] <- screen_set
        solver_args[["screen_beta"]] <- screen_beta
        solver_args[["screen_is_active"]] <- screen_is_active
        solver_args[["screen_dual"]] <- screen_dual
        solver_args[["active_set_size"]] <- active_set_size
        solver_args[["active_set"]] <- active_set

        # special gaussian case
        if (is_gaussian_opt) {
            if (is.null(warm_start)) {
                ones <- rep_len(1.0, n)
                X_means <- X$mul(ones, weights)
                y_off <- y - offsets
                y_mean <- sum(y_off * weights)
                yc <- y_off
                if (intercept) {
                    yc <- yc - y_mean
                }
                y_var <- sum(weights * yc ** 2)
                rsq <- 0.0
                resid <- yc
                resid_sum <- sum(weights * resid)
                grad <- X$mul(resid, weights)
            } else {
                X_means <- as.double(warm_start$X_means)
                y_mean <- as.double(warm_start$y_mean)
                y_var <- as.double(warm_start$y_var)
                rsq <- as.double(warm_start$rsq)
                resid <- as.double(warm_start$resid)
                resid_sum <- as.double(warm_start$resid_sum)
                grad <- as.double(warm_start$grad)
            }

            solver_args[["X_means"]] <- X_means
            solver_args[["y_mean"]] <- y_mean
            solver_args[["y_var"]] <- y_var
            solver_args[["rsq"]] <- rsq
            solver_args[["resid"]] <- resid
            solver_args[["resid_sum"]] <- resid_sum
            solver_args[["grad"]] <- grad

            state <- do.call(state.gaussian_naive, solver_args)

        # GLM case
        } else {
            if (is.null(warm_start)) {
                ones <- rep_len(1.0, n)
                beta0 <- 0.0
                eta <- as.double(offsets)
                resid <- glm$gradient(eta)
                grad <- X$mul(resid, ones)
                loss_null <- NULL
                loss_full <- as.double(glm$loss_full())
            } else {
                beta0 <- as.double(warm_start$beta0)
                eta <- as.double(warm_start$eta)
                resid <- as.double(warm_start$resid)
                grad <- as.double(warm_start$grad)
                loss_null <- as.double(warm_start$loss_null)
                loss_full <- as.double(warm_start$loss_full)
            }

            solver_args[["beta0"]] <- beta0
            solver_args[["grad"]] <- grad
            solver_args[["eta"]] <- eta
            solver_args[["resid"]] <- resid
            solver_args[["loss_null"]] <- loss_null
            solver_args[["loss_full"]] <- loss_full
            state <- do.call(state.glm_naive, solver_args)
        }
    }

    solve_(
        state=state,
        progress_bar=progress_bar
    )
}