#' Uncorrected Sum of Squares
#'
#' Computes the uncorrected sum of squares of a numeric vector.
#' This is equivalent to the sum of the squared values and does not
#' adjust for the mean.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing the sum of squared elements in
#'   \code{x}, with missing values removed.
#'
#' @details
#' The uncorrected sum of squares is calculated as:
#'
#' \deqn{\sum x_i^2}
#'
#' This measure is commonly used in statistical calculations and differs
#' from the corrected sum of squares because it does not account for the
#' mean of the data.
#'
#' @examples
#' ssq(c(8, 2, 5))
#' # Returns 93
#'
#' ssq(c(1, 2, 3, NA))
#' # Returns 14
#'
#' @export
#' 
ssq <- function(x) {sum(x * x, na.rm=TRUE)}


#' Corrected Sum of Squares
#'
#' Computes the corrected sum of squares of a numeric vector.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing the sum of squared deviations
#'   from the mean.
#'
#' @details
#' The corrected sum of squares is defined as:
#'
#' \deqn{\sum (x_i - \bar{x})^2}
#'
#' This quantity forms the basis of variance calculations and analysis
#' of variance (ANOVA).
#'
#' Missing values are removed before computation.
#'
#' @examples
#' cssq(c(2, 8, 5))
#' # Returns 18
#'
#' cssq(c(1, 2, 3, 4))
#' # Returns 5
#'
#' @export
#' 
cssq <- function(x) {sum((x - mean(x))^2 ,na.rm=TRUE)}   


#' Sum of Digits
#'
#' Computes the sum of the digits for each integer value.
#'
#' @param x A numeric or integer vector.
#'
#' @return A numeric vector containing the sum of digits for each element.
#'
#' @details
#' Each number is converted to its individual digits and these digits
#' are summed.
#'
#' This function is intended to provide functionality similar to
#' \code{DescTools::DigitSum()}, although the behaviour may differ for
#' 
digitsum <- function(x) {sapply(x ,function(n){a<-as.integer(c(strsplit(as.character(n),split="")[[1]])); sum(a)})}


#' Squares of Digits
#'
#' Computes the square of each digit in an integer.
#'
#' @param x A numeric or integer vector.
#'
#' @return A list containing the squared digits for each input value.
#'
#' @details
#' Each element is split into its constituent digits and each digit is
#' squared individually.
#'
#' @examples
#' digitsqu(c(8, 23, 471))
#' # Returns:
#' # 64
#' # c(4, 9)
#' # c(16, 49, 1)
#'
#' @export
#' 
digitsqu <- function(x) {
  lapply(x, function(n) {
    a <- as.integer(strsplit(as.character(n), "")[[1]])
    a^2
  })
}

#' Digit Sum of Squares
#'
#' Computes the sum of squared digits for each integer value.
#'
#' @param x A numeric or integer vector.
#'
#' @return A numeric vector containing the sum of the squared digits.
#'
#' @details
#' Each number is decomposed into its digits, each digit is squared,
#' and the resulting values are summed.
#'
#' @examples
#' digitssq(c(8, 23, 471))
#' # Returns c(64, 13, 66)
#'
#' @export
#' 
digitssq <- function(x) {sapply(x ,function(n){a<-as.integer(c(strsplit(as.character(n),split="")[[1]])); sum(a^2)})}


#' Pascal Triangle Coefficients
#'
#' Generates binomial coefficients corresponding to rows of Pascal's Triangle.
#'
#' @param x A vector of non-negative integers.
#'
#' @return A list containing the binomial coefficients for each supplied
#'   integer.
#'
#' @details
#' For each value \code{n}, the function calculates:
#'
#' \deqn{\binom{n}{0}, \binom{n}{1}, \ldots, \binom{n}{n}}
#'
#' These values correspond to the rows of Pascal's Triangle.
#'
#' @examples
#' pascal(c(1, 2, 3, 4))
#'
#' # Returns:
#' # c(1, 1)
#' # c(1, 2, 1)
#' # c(1, 3, 3, 1)
#' # c(1, 4, 6, 4, 1)
#'
#' @export
#' 
pascal <- function(x) {
  sapply(x, function(x) {
    lapply(x, function(i) {
      choose(i, 0:i)
    })
  })
}

#' Fraction Representation
#'
#' Converts decimal values to fractional representations.
#'
#' @param x A numeric vector.
#'
#' @return A character vector containing fractional representations of
#'   the supplied values.
#'
#' @details
#' This function is a wrapper around \code{MASS::fractions()} and returns
#' the results as character strings rather than fraction objects.
#'
#' @examples
#' fractions(c(0.75, 2.3, 0.28))
#' # Returns c("3/4", "23/10", "7/25")
#'
#' @seealso
#' \code{\link{decimals}}
#'
#' @export
#' 
fractions <- function(x) {as.character(MASS::fractions(x))}


#' Decimal Representation
#'
#' Converts fractional character strings into decimal values.
#'
#' @param x A character vector containing fractions expressed as strings.
#'
#' @return A numeric vector containing the decimal equivalents of the
#'   supplied fractions.
#'
#' @details
#' Fraction strings are evaluated and converted to their corresponding
#' decimal values.
#'
#' This function is intended as the inverse operation of
#' \code{\link{fractions}}.
#'
#' @examples
#' decimals(c("3/4", "23/10", "7/25"))
#' # Returns c(0.75, 2.3, 0.28)
#'
#' fractions(decimals(c("3/4", "23/10")))
#' # Returns c("3/4", "23/10")
#'
#' @seealso
#' \code{\link{fractions}}
#'
#' @export
#' 
decimals <- function(x) {
  unname(sapply(x, function(w) eval(parse(text = w))))
}
