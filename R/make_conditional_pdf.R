#' Estimates a empirical conditional density function
#'
#' Given two continuous variables, this function estimates their empirical conditional
#' distribution via kernel density smoothing.
#'
#' @param x Numeric. A continuous variable.
#' @param given_y Numeric. A continuous variable on which the distribution of \code{x} will be conditioned on
#' @param w Numeric. Frequency Weights.
#' @return Returns a bivariate function which evaluates to the empirical distribution of \code{x} conditional on \code{given_y}
#' @import data.table
#' @import ks
#' @import mvQuad
#' @import pracma
#' @export
make_conditional_pdf <- function(x, given_y, w = NULL) {

        if(length(x) != length(given_y)){
                stop("x and given_y must have the same length")
        }

        data = data.table(x, given_y)

        if(is.null(w)){
                data[ , w := rep(1, length(x))]
        }else{
                if(length(w) != length(given_y)){
                        stop("w must have the same length as x and given_y")
                }
                data[ , w := w]
        }

        data = data[complete.cases(data)]

        # Compute the joint probability function P(x, y)
        joint_pdf <- make_bivariate_pdf(x = data$x, y = data$given_y, w = data$w)


        # Function to compute the marginal probability P(y) for a given value of y
        marginal_probability_y <- function(given_y) {

                pdf_integration <- function(x) {
                        return(joint_pdf(x, given_y))
                }
                lower <- min(data$x)
                upper <- max(data$x)

                grid = mvQuad::createNIGrid(1, type = "GLe", level = 300)
                mvQuad::rescale(object = grid, domain = cbind(lower, upper))
                mvQuad::quadrature(f = pdf_integration, grid = grid)
        }
        marginal_probability_y <- Vectorize(marginal_probability_y)

        # Function to evaluate the conditional probability P(x|y) at a given point (x, given_y)
        conditional_pdf <- function(x, given_y) {

                joint_probability       <- joint_pdf(x, given_y)
                marginal_probability    <- marginal_probability_y(given_y)
                conditional_probability <- joint_probability / marginal_probability

                return(conditional_probability)
        }

        return(conditional_pdf)
}

