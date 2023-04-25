#' Returns a function which calculates Mean Effect of INE
#'
#' @param reg a glm binary logistic regression object
#' @param income_var A character vector with the name of the income variable,
#'   as it appear in \code{all.vars(formula(reg))}.
#' @param performance_var A character vector with the name of the schooling
#'   performance variable, as it appear in \code{all.vars(formula(reg))}.
#' @return Returns a bivariate function which calculates the INE mean effect
#'   between two points/observations on the income distribution.
#' @import data.table
#' @import ks
#' @import mvQuad
#' @export
make_INE_meanEffect = function(reg, income_var, performance_var){

        if(!"glm" %in% class(reg)){
                stop("reg must be a glm object")
        }

        if(!class(income_var) == "character"|!class(performance_var) == "character"){
                stop("income_var and performance_var must be characters, both with length 1")
        }

        if(length(income_var) != 1|length(performance_var) != 1){
                stop("income_var and performance_var must be characters, both with length 1")
        }

        vars_of_interest = c(income_var, performance_var)
        data = model.frame(reg)

        P = make_P_ip(reg = reg, vars_of_interest = vars_of_interest)
        f = make_conditional_pdf(x = data[[performance_var]], given_y = data[[income_var]], w = reg$prior.weights)

        function(U,L){

                meanEffect_tmp = function(p){

                        delta_P = function(p) P(U, p) - P(L, p)
                        f_bar   = function(p) (f(p, U) + f(p, L))/2

                        delta_P(p)*f_bar(p)
                }

                lower <- min(data[[performance_var]])
                upper <- max(data[[performance_var]])

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))
                mvQuad::quadrature(f = meanEffect_tmp, grid = grid)
        }
}

