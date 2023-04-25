#' Makes a bivariate function that produces predicted probabilities of a Logistic Regression (glm)
#'
#' A wrapper for producing predicted values for a logistic regression by returnin a
#' function with two inputs, performance and income, and setting all the other covariates
#' to their respective mean.
#'
#' @param reg a glm binary logistic regression object
#' @param vars_of_interest A character vector with names of the schooling performance and income variables, as they appear in \code{all.vars(formula(reg))}. The other variables used in the glm estimation will be regarded as covariates.
#' @return Returns a bivariate function which produces predicted values for \code{reg}, given the performance and income level passed to the function. All covariates are set to they mean (or mode)
#' @import data.table
#' @import ks
#' @import mvQuad
#' @import pracma
#' @export
make_P_ip = function(reg, vars_of_interest){

        if(!"glm" %in% class(reg)){
                stop("reg must be a glm object")
        }

        if(!class(vars_of_interest) == "character"){
                stop("vars_of_interest must be a character with with names of the schooling performance and income variables")
        }

        if(length(vars_of_interest) != 2){
                stop("vars_of_interest must be a character with exactly 2 values: the names of schooling performance and income variables")
        }


        # Create the function body
        function_body <- quote({

                control_vars = setdiff(all.vars(formula(reg))[-1], vars_of_interest)

                data_for_predicton <- list()
                for (vars_of_interest_i in vars_of_interest) {
                        data_for_predicton[[vars_of_interest_i]] <- get(vars_of_interest_i)
                }

                data_for_predicton <- as.data.frame(data_for_predicton)

                if(length(control_vars) > 0){
                        for(control_i in control_vars){

                                if(is.numeric(reg$data[[control_i]])){
                                        data_for_predicton[[control_i]] <- weighted.mean(x = reg$data[[control_i]], reg$prior.weights)
                                }else{
                                        data_for_predicton[[control_i]] <- mode(x = reg$data[[control_i]], w = reg$prior.weights)
                                }

                                class(data_for_predicton[[control_i]]) <- class(reg$data[[control_i]])
                        }
                }

                predicted = predict(object = reg, newdata = data_for_predicton, type = "response")

                return(predicted)

        })

        # Evaluate the call to create the function
        expr = paste(c("c(alist(",
                       paste(paste(vars_of_interest, "= "), collapse = ", "),
                       "), list(function_body))"
        ), collapse = "")
        function_call = eval(parse(text = expr))

        new_function = as.function(function_call)

        environment(new_function)$reg              <- reg
        environment(new_function)$vars_of_interest <- vars_of_interest

        return(new_function)
}

