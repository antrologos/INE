#' Estimates a bivariate empirical density function
#'
#' Given two continuous variables, this function estimates their empirical joint
#' distribution via kernel density smoothing.
#'
#' @param x Numeric. A continuous variable.
#' @param y Numeric. A continuous variable.
#' @param w Numeric. Frequency Weights.
#' @return Returns a bivariate function which evaluates to the empirical joint distribution
#' @import data.table
#' @import ks
#' @import mvQuad
#' @import pracma
#' @export
make_bivariate_pdf <- function(x, y, w = NULL) {

        if(length(x) != length(y)){
                stop("x and y must have the same length")
        }

        data = data.table::data.table(x, y)

        if(is.null(w)){
                data[ , w := rep(1, length(x))]
        }else{
                if(length(w) != length(y)){
                        stop("w must have the same length as x and y")
                }
                data[ , w := w]
        }

        data = data[complete.cases(data)]


        # Estimate bivariate kernel density
        H = ks::Hpi(as.matrix(data[,.(x, y)]))  # Optimal bandwidth selection
        kde_result <- ks::kde(x = as.matrix(data[,.(x, y)]),
                          w = data$w,
                          H = H,
                          density = T)

        # Function to evaluate the pdf at a given point
        eval_pdf_tmp <- function(x, y) {
                density <- ks:::predict.kde(kde_result, x = cbind(x, y))
                return(density)
        }

        total_probability = pracma::integral2(Vectorize(eval_pdf_tmp),
                                              xmin = min(x) - 3 * H[1],
                                              ymin = min(y) - 3 * H[2],
                                              xmax = max(x) + 3 * H[1],
                                              ymax = max(y) + 3 * H[2])$Q

        eval_pdf <- function(x, y) {
                density <- ks:::predict.kde(kde_result, x = cbind(x, y))/total_probability
                return(density)
        }

        # Store objects in the environment of the eval_pdf function
        environment(eval_pdf)$kde_result        <- kde_result
        environment(eval_pdf)$total_probability <- total_probability

        return(eval_pdf)
}


