#' Finding the greatest common divisor (GCD)
#' 
#' This function finds the GCD by using modulus function and while loop
#' 
#' @param a integer
#' @param b integer
#' @return an integer equal to GCD
#' @author Reuel, Martin, Vinay
#' @source \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean algorithm}
#' @export

euclidean <- function(a, b)
{
      while (b != 0)
      {
      t = b 
      b = a %% b
      a = t
      }
  return (a)
}


