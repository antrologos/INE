#' Returns a function which calculates Mean Effect of the Comparative Advantage (CA)
#'
#' @param reg a glm binary logistic regression object
#' @param income_var A character vector with the name of the income variable,
#'   as it appear in \code{all.vars(formula(reg))}.
#' @param performance_var A character vector with the name of the schooling
#'   performance variable, as it appear in \code{all.vars(formula(reg))}.
#' @return Returns a bivariate function which calculates the CA Mean effect
#'   between two points/observations on the income distribution.
#' @import data.table
#' @import ks
#' @import mvQuad
#' @export
make_CA_meanEffect = function(reg, income_var, performance_var, good_perfomance = NULL){

        if(!"glm" %in% class(reg)){
                stop("reg must be a glm object")
        }

        if(!class(income_var) == "character"|!class(performance_var) == "character"){
                stop("income_var and performance_var must be characters, both with length 1")
        }

        if(length(income_var) != 1|length(performance_var) != 1){
                stop("income_var and performance_var must be characters, both with length 1")
        }

        if(!is.null(good_perfomance)){

                if(!is.numeric(good_perfomance)){
                        stop("good_perfomance must be numeric")
                }

                if(length(good_perfomance) != 1){
                        stop("good_perfomance must be length 1")
                }
        }

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

                INE_o_meanEff_tmp = function(p){

                        delta_P = function(p) P(U, p) - P(L, p)
                        f_bar   = function(p) (f(p, U) + f(p, L))/2

                        delta_P(p)*f_bar(p)

                }

                INE_sim_meanEff_tmp = function(p){

                        delta_P_sim = function(p) min(P(L, p) + (P(U,G) - P(L, G)), 1) - P(L, p)
                        f_bar   = function(p) (f(p, U) + f(p, L))/2

                        delta_P_sim(p)*f_bar(p)

                }

                lower <- min(data[[performance_var]])
                upper <- max(data[[performance_var]])

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))

                INE_o       = mvQuad::quadrature(f = INE_tmp, grid = grid)
                meanEff_o   = mvQuad::quadrature(f = INE_o_meanEff_tmp, grid = grid)
                meanEff_sim = mvQuad::quadrature(f = INE_sim_meanEff_tmp, grid = grid)

                (meanEff_o - meanEff_sim)/INE_o
        }
}
