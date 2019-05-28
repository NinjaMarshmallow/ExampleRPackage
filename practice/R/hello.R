# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Square Function
#'
#' @param x the vector to be squared
#'
#' @return The vector with all elements squared
#' @export
#' @note: This function is vectorized
#' @examples
#' square(9) outputs 81
#' square(-7) outputs 49
#' square(c(1, 5, 8)) outputs [ 1 25 64 ]
square <- function(x) {
  if(is.numeric(x)) {
    return(x ** 2)
  }
  stop("The parameter given was non-numeric. Please pass a numeric value")
}

#' Factorial Function
#'
#' @param x the object to be factorialized
#'
#' @return the resulting vector of factorialized numbers
#' @export
#' @note This function is vectorized
#' @examples
#' factorial2(2) outputs 4
#' factorial2(4) outputs 24
#' factorial2(c(0, 3, 5)) outputs [ 1 6 120 ]
factorial2 <- function(x) {
  if(!is.numeric(x)) {
    stop("The parameter must be numeric")
  }
  if(length(x) > 1) {
    return(sapply(x, factorial2))
  }
  if(x < 0) {
    stop("The parameter must be non-negative")
  }
  if(x == 0 || x == 1) {
    return(1)
  }
  result <- x
  total <- 1
  while(result > 1) {
    total <- total * result
    result <- result - 1
  }
  return(total)
}

#' Fibonacci Function
#'
#' @param x the index of the desired fibonacci number
#'
#' @return vector of the fibonacci number of the indeces given
#' @export
#' @note This function is vectorized
#' @examples
#' fib(2) outputs 1
#' fib(4) outputs 3
#' fib(c(1, 5, 8)) outputs [ 1 5 21 ]
fib <- function(x) {
  if(x < 1) {
    stop("The parameter must be positive")
  }
  if(x < 3) {
    return(1)
  }
  return(fib(x - 1) + fib(x - 2))
}

#' Fast Fibonacci Function
#'
#' @param x the index of the desired fibonacci number
#' @description Fast Fibonacci executes in constant time by using a formula instead of recursion
#' @return vector of the fibonacci number of the indeces given
#' @export
#' @note This function is vectorized
#' @examples
#' fib(2) outputs 1
#' fib(4) outputs 3
#' fib(c(1, 5, 8)) outputs [ 1 5 21 ]
fastfib <- function(x) {
  if(x < 1) {
    stop("The parameter must be positive")
  }
  if(x < 3) {
    return(1)
  }
  a <- (1 + sqrt(5))/2
  b <- (1 - sqrt(5))/2
  (a ** x - b ** x)/sqrt(5)
}

#' Divisors Of
#'
#' @param x the number to find the divisors of
#'
#' @return Returns a vector of divisors, must include 1 and the x itself if x was a single integer.
#' Returns a list of vectors if x was a vector of integers
#' @note This function is not exactly vectorized as passing a vector returns a list of vectors
#' @export
#'
#' @examples
#' divisors_of(12) outputs [ 1, 2, 3, 4, 6, 12 ]
#' divisors_of(7) outputs  [ 1, 7 ]
#' divisors_of(c(10, 14, 5)) outputs list( [ 1, 2, 5, 10], [ 1, 2, 7, 14 ], [ 1, 5 ] )
divisors_of <- function(x) {
  if(!is.numeric(x)) {
    stop("The parameter must be numeric")
  }
  if(!is.integer(x)) {
    as.integer(x)
  }
  if(length(x) > 1) {
    return(lapply(x, divisors_of))
  }
  if(x < 1) {
    stop("The integer must be positive")
  }
  if(x == 1) {
    return(1)
  }
  divisors <- c(1)
  current <- 2
  if(x > 0) {
    while(current <= x / 2) {
      if(x / current == x %/% current ) {
        divisors <- c(divisors, current)
      }
      current <- current + 1
    }
    c(divisors, x)
  }
}






