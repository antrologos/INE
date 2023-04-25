#' Returns a function which calculates the Educational Inequality (INE) between 2 income levels
#'
#' Returns a function which calculates the Educational Inequality (INE) between 2 income levels
#'
#' @param reg a glm binary logistic regression object
#' @param income_var A character vector with the name of the income variable,
#'   as it appear in \code{all.vars(formula(reg))}.
#' @param performance_var A character vector with the name of the schooling
#'   performance variable, as it appear in \code{all.vars(formula(reg))}.
#' @return Returns a bivariate function which calculates the difference in
#'   probability of a successful educational transition (as modelled by \code{reg})
#'   between two points/observations on the income distribution. Schooling perfomance
#'   is already integrated out and all covariated set to their respective mean.
#' @import data.table
#' @import ks
#' @import mvQuad
#' @export
make_INE = function(reg, income_var, performance_var){

        if(!"glm" %in% class(reg)){
                stop("reg must be a glm object")
        }

        if(!class(income_var) == "character"|!class(performance_var) == "character"){
                stop("income_var and performance_var must be characters, both with length 1")
        }

        if(!length(income_var) != 1|!length(performance_var) != 1){
                stop("income_var and performance_var must be characters, both with length 1")
        }

        vars_of_interest = c(income_var, performance_var)
        data = model.frame(reg)

        P = make_P_ip(reg = reg, vars_of_interest = vars_of_interest)
        f = make_conditional_pdf(x = data[[performance_var]], given_y = data[[income_var]], w = reg$prior.weights)

        function(U,L){

                INE_tmp = function(p){
                        P(U, p)*f(p, U) - P(L, p)*f(p, L)
                }

                lower <- min(data[[performance_var]])
                upper <- max(data[[performance_var]])

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))
                mvQuad::quadrature(f = INE_tmp, grid = grid)
        }
}

