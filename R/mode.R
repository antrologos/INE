#' Mode (most frequent value) of a distribution
#'
#' Identifies and returns the most frequent value of a variable.
#'
#' @param x A numeric, character, or factor vector.
#' @param w Numeric. Frequency weights
#' @return Returns a single-valued vector of the same class as x.
#' @import data.table
#' @export
mode <- function(x, w = NULL){

        dt = data.table(x)

        if(is.null(w)){
                dt[, w := 1]
        }else{
                if(length(w) != length(x)){
                        stop("x and w must be the same length.")
                }
                dt[["w"]] <- w
        }

        dt[complete.cases(dt)][, .(N = sum(w)), by = x][order(N, decreasing = T)][1, x]
}

