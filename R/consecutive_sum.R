#' Consecutive Sum
#'
#' @description This function calculates the consecutive sum of a given vector.
#'
#' @param x A numeric vector.
#' @param initial_value A numeric value representing the initial sum value. Default is NA.
#'
#' @return A numeric vector with the consecutive sum values.
#' @export
#'
#' @examples
#' consecutive_sum(c(1, 2, 3, 4, 5))
#' 
#' consecutive_sum(c(1, NA, 3, 0, 5, 6), initial_value = 10)
consecutive_sum <- function(x, initial_value = 0){
  out = x
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      if(x[i] != 0){
        if(i > 1){
          out[i]=x[i] + out[i-1]
        } else{
          out[i] = x[i] + initial_value
        }
      } 
    }
  }
  return(out)
}