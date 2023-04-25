#' Returns a function which calculates the Comparative Advantage (CA) between 2 income levels
#'
#' @param reg a glm binary logistic regression object
#' @param income_var A character vector with the name of the income variable,
#'   as it appear in \code{all.vars(formula(reg))}.
#' @param performance_var A character vector with the name of the schooling
#'   performance variable, as it appear in \code{all.vars(formula(reg))}.
#' @param good_perfomance Numeric.
#' @return Returns a bivariate function which calculates the comparative advantage
#'   between two points/observations on the income distribution.
#' @import data.table
#' @import ks
#' @import mvQuad
#' @export
make_CA = function(reg, income_var, performance_var, good_perfomance = NULL){

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

                INE_o_tmp = function(p){
                        P(U, p)*f(p, U) - P(L, p)*f(p, L)
                }

                INE_sim_tmp = function(p){

                        P_U_sim = P(L, p) + (P(U,G) - P(L, G))

                        min(P_U_sim, 1)*f(p, U) - P(L, p)*f(p, L)
                }

                lower <- min(data[[performance_var]])
                upper <- max(data[[performance_var]])

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))

                INE_o   = mvQuad::quadrature(f = INE_o_tmp, grid = grid)
                INE_sim = mvQuad::quadrature(f = INE_sim_tmp, grid = grid)

                (INE_o - INE_sim)/INE_o
        }
}
