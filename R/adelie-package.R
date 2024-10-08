#' @import Rcpp
#' @import Matrix
#' @import r2r
#' @importFrom methods as new
#' @importFrom stats predict coef approx weighted.mean
#' @importFrom graphics abline axis legend matplot points segments
#' @useDynLib adelie, .registration = TRUE
NULL
Rcpp::loadModule("adelie_core_configs", TRUE)
Rcpp::loadModule("adelie_core_constraint", TRUE)
Rcpp::loadModule("adelie_core_glm", TRUE)
Rcpp::loadModule("adelie_core_io", TRUE)
Rcpp::loadModule("adelie_core_matrix", TRUE)
Rcpp::loadModule("adelie_core_matrix_utils_blas", TRUE)
Rcpp::loadModule("adelie_core_solver", TRUE)
Rcpp::loadModule("adelie_core_state", TRUE)
