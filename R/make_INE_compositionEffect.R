#' Returns a function which calculates Composition Effect of INE
#'
#' @param reg a glm binary logistic regression object
#' @param income_var A character vector with the name of the income variable,
#'   as it appear in \code{all.vars(formula(reg))}.
#' @param performance_var A character vector with the name of the schooling
#'   performance variable, as it appear in \code{all.vars(formula(reg))}.
#' @return Returns a bivariate function which calculates the INE Composition effect
#'   between two points/observations on the income distribution.
#' @import data.table
#' @import ks
#' @import mvQuad
#' @export
make_INE_compositionEffect = function(reg, income_var, performance_var){

        vars_of_interest = c(income_var, performance_var)
        data = model.frame(reg)

        P = make_P_ip(reg = reg, vars_of_interest = vars_of_interest)
        f = make_conditional_pdf(x = data[[performance_var]], given_y = data[[income_var]], w = reg$prior.weights)

        function(U,L){

                compositionEffect_tmp = function(p){

                        P_bar   = function(p) (P(U, p) + P(L, p))/2
                        delta_f = function(p) f(p, U) - f(p, L)

                        P_bar(p)*delta_f(p)
                }

                lower <- min(data[[performance_var]])
                upper <- max(data[[performance_var]])

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))
                mvQuad::quadrature(f = compositionEffect_tmp, grid = grid)
        }
}

