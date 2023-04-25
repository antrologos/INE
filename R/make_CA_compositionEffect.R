#' Returns a function which calculates Composition Effect of the Comparative Advantage (CA)
#'
#' @param reg a glm binary logistic regression object
#' @param income_var A character vector with the name of the income variable,
#'   as it appear in \code{all.vars(formula(reg))}.
#' @param performance_var A character vector with the name of the schooling
#'   performance variable, as it appear in \code{all.vars(formula(reg))}.
#' @return Returns a bivariate function which calculates the CA Composition effect
#'   between two points/observations on the income distribution.
#' @import data.table
#' @import ks
#' @import mvQuad
#' @export
make_CA_compositionEffect = function(reg, income_var, performance_var, good_perfomance = NULL){

        vars_of_interest = c(income_var, performance_var)
        data = model.frame(reg)

        P = make_P_ip(reg = reg, vars_of_interest = vars_of_interest)
        f = make_conditional_pdf(x = data[[performance_var]], given_y = data[[income_var]], w = reg$prior.weights)

        if(is.null(good_perfomance)){
                G = max(data[[performance_var]])
        }else{
                G = good_perfomance
        }

        function(U,L){

                INE_tmp = function(p){
                        P(U, p)*f(p, U) - P(L, p)*f(p, L)
                }

                INE_o_compositionEffect = function(p){
                        P_bar   = function(p) (P(U, p) + P(L, p))/2
                        delta_f = function(p) f(p, U) - f(p, L)

                        P_bar(p)*delta_f(p)
                }

                INE_sim_compositionEffect = function(p){

                        P_sim_bar = function(p) (min(P(L, p) + (P(U,G) - P(L, G)), 1) + P(L, p))/2
                        delta_f   = function(p) f(p, U) - f(p, L)

                        P_sim_bar(p)*delta_f(p)
                }

                lower <- min(data[[performance_var]])
                upper <- max(data[[performance_var]])

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))

                INE_o       = mvQuad::quadrature(f = INE_tmp, grid = grid)
                compEff_o   = mvQuad::quadrature(f = INE_o_compositionEffect, grid = grid)
                compEff_sim = mvQuad::quadrature(f = INE_sim_compositionEffect, grid = grid)

                (compEff_o - compEff_sim)/INE_o
        }
}
