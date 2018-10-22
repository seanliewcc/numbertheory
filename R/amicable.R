#' Check if two numbers are amicable.
#'
#' Does not have any dependencies, but for assignment purposes will suggest dplyr to be installed.
#'
#' Two distinct positive integers are amicable if their sum of proper divisors of each are equal to the other number.
#'
#' @param x The first number.
#' @param y The second number.
#' @return A logical TRUE or FALSE.
#' @examples
#' amicable(220, 284)
#' amicable(10, 12)
#' @export

amicable <- function(x,y) { # no defaults since the user should input two distinct numbers
  if (x == y | x < 1 | y < 1 | x%%1 != 0 | y%%1 != 0) stop ("x and y must be distinct positive integers")
  factors_x <- (1:x)[x %% (1:x) == 0]
  factors_y <- (1:y)[y %% (1:y) == 0]
  sum(factors_x) == y & sum(factors_y) == x
}
