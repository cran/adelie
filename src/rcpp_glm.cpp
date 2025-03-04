#include "rcpp_glm.h"

using value_t = double;
using index_t = int;
using vec_value_t = ad::util::colvec_type<value_t>;
using vec_index_t = ad::util::colvec_type<index_t>;
using rowarr_value_t = ad::util::rowarr_type<value_t>;
using colarr_value_t = ad::util::colarr_type<value_t>;

/* Factory functions */

auto make_r_glm_binomial_logit_64(Rcpp::List args)
{
    const Eigen::Map<vec_value_t> y = args["y"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    return new r_glm_binomial_logit_64_t(y, weights);
}

auto make_r_glm_binomial_probit_64(Rcpp::List args)
{
    const Eigen::Map<vec_value_t> y = args["y"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    return new r_glm_binomial_probit_64_t(y, weights);
}

auto make_r_glm_cox_64(Rcpp::List args)
{
    const Eigen::Map<vec_value_t> start = args["start"];
    const Eigen::Map<vec_value_t> stop = args["stop"];
    const Eigen::Map<vec_value_t> status = args["status"];
    const Eigen::Map<vec_index_t> strata = args["strata"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    const std::string tie_method = args["tie_method"];
    return new r_glm_cox_64_t(start, stop, status, strata, weights, tie_method);
}

auto make_r_glm_gaussian_64(Rcpp::List args)
{
    const Eigen::Map<vec_value_t> y = args["y"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    return new r_glm_gaussian_64_t(y, weights);
}

auto make_r_glm_poisson_64(Rcpp::List args)
{
    const Eigen::Map<vec_value_t> y = args["y"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    return new r_glm_poisson_64_t(y, weights);
}

auto make_r_glm_s4_64(Rcpp::List args)
{
    Rcpp::S4 glm = args["glm"];
    const Eigen::Map<vec_value_t> y = args["y"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    return new r_glm_s4_64_t(glm, y, weights);
}

auto make_r_glm_multigaussian_64(Rcpp::List args)
{
    const Eigen::Map<colarr_value_t> yT = args["yT"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    Eigen::Map<const rowarr_value_t> y(yT.data(), yT.cols(), yT.rows());
    return new r_glm_multigaussian_64_t(y, weights);
}

auto make_r_glm_multinomial_64(Rcpp::List args)
{
    const Eigen::Map<colarr_value_t> yT = args["yT"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    Eigen::Map<const rowarr_value_t> y(yT.data(), yT.cols(), yT.rows());
    return new r_glm_multinomial_64_t(y, weights);
}

auto make_r_glm_multis4_64(Rcpp::List args)
{
    Rcpp::S4 glm = args["glm"];
    const Eigen::Map<colarr_value_t> yT = args["yT"];
    const Eigen::Map<vec_value_t> weights = args["weights"];
    Eigen::Map<const rowarr_value_t> y(yT.data(), yT.cols(), yT.rows());
    return new r_glm_multis4_64_t(glm, y, weights);
}

RCPP_MODULE(adelie_core_glm)
{
    /* base classes */
    Rcpp::class_<r_glm_base_64_t>("RGlmBase64")
        .method("gradient", &r_glm_base_64_t::gradient)
        .method("hessian", &r_glm_base_64_t::hessian)
        .method("loss", &r_glm_base_64_t::loss)
        .method("loss_full", &r_glm_base_64_t::loss_full)
        .method("inv_link", &r_glm_base_64_t::inv_link)
        .property("is_multi", &r_glm_base_64_t::is_multi)
        .property("name", &r_glm_base_64_t::name)
        .property("y", &r_glm_base_64_t::y)
        .property("weights", &r_glm_base_64_t::weights)
        ;

    Rcpp::class_<r_glm_multibase_64_t>("RGlmMultiBase64")
        .method("gradient", &r_glm_multibase_64_t::gradient)
        .method("hessian", &r_glm_multibase_64_t::hessian)
        .method("loss", &r_glm_multibase_64_t::loss)
        .method("loss_full", &r_glm_multibase_64_t::loss_full)
        .method("inv_link", &r_glm_multibase_64_t::inv_link)
        .property("is_multi", &r_glm_multibase_64_t::is_multi)
        .property("name", &r_glm_multibase_64_t::name)
        .property("y", &r_glm_multibase_64_t::y)
        .property("weights", &r_glm_multibase_64_t::weights)
        ;

    /* GLM classes */
    Rcpp::class_<r_glm_binomial_logit_64_t>("RGlmBinomialLogit64")
        .derives<r_glm_base_64_t>("RGlmBase64")
        .factory<Rcpp::List>(make_r_glm_binomial_logit_64)
        ;
    Rcpp::class_<r_glm_binomial_probit_64_t>("RGlmBinomialProbit64")
        .derives<r_glm_base_64_t>("RGlmBase64")
        .factory<Rcpp::List>(make_r_glm_binomial_probit_64)
        ;
    Rcpp::class_<r_glm_cox_64_t>("RGlmCox64")
        .derives<r_glm_base_64_t>("RGlmBase64")
        .factory<Rcpp::List>(make_r_glm_cox_64)
        ;
    Rcpp::class_<r_glm_gaussian_64_t>("RGlmGaussian64")
        .derives<r_glm_base_64_t>("RGlmBase64")
        .factory<Rcpp::List>(make_r_glm_gaussian_64)
        ;
    Rcpp::class_<r_glm_poisson_64_t>("RGlmPoisson64")
        .derives<r_glm_base_64_t>("RGlmBase64")
        .factory<Rcpp::List>(make_r_glm_poisson_64)
        ;
    Rcpp::class_<r_glm_s4_64_t>("RGlmS464")
        .derives<r_glm_base_64_t>("RGlmBase64")
        .factory<Rcpp::List>(make_r_glm_s4_64)
        ;
    Rcpp::class_<r_glm_multigaussian_64_t>("RGlmMultiGaussian64")
        .derives<r_glm_multibase_64_t>("RGlmMultiBase64")
        .factory<Rcpp::List>(make_r_glm_multigaussian_64)
        ;
    Rcpp::class_<r_glm_multinomial_64_t>("RGlmMultinomial64")
        .derives<r_glm_multibase_64_t>("RGlmMultiBase64")
        .factory<Rcpp::List>(make_r_glm_multinomial_64)
        ;
    Rcpp::class_<r_glm_multis4_64_t>("RGlmMultiS464")
        .derives<r_glm_multibase_64_t>("RGlmMultiBase64")
        .factory<Rcpp::List>(make_r_glm_multis4_64)
        ;
}
