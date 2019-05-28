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
#' @description the square function raises x to the second power
#' @param x the vector to be squared
#' @return The vector with all elements squared
#' @section Quick Reference:
#' \preformatted{square(9) outputs 81
#' square(-7) outputs 49
#' square(c(1, 5, 8)) outputs [ 1 25 64 ]}
#' @export
#' @note: This function is vectorized
#' @examples
#' square(9)
#' square(-7)
#' square(c(1, 5, 8))
square <- function(x) {
  if(is.numeric(x)) {
    return(x ** 2)
  }
  stop("The parameter given was non-numeric. Please pass a numeric value")
}

#' Factorial Function
#' @description the factorial2 function returns the the factorial of the given number
#' @param x the object to be factorialized
#'
#' @return the resulting vector of factorialized numbers
#' @section Quick Reference:
#' \preformatted{factorial2(2) outputs 4
#' factorial2(4) outputs 24
#' factorial2(c(0, 3, 5)) outputs [ 1, 6, 120 ]}
#' @export
#' @note This function is vectorized
#' @examples
#' factorial2(2)
#' factorial2(4)
#' factorial2(c(0, 3, 5))

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
#' @description The fib function calculates the fibonacci number at the given index
#' @param x the index of the desired fibonacci number
#'
#' @return vector of the fibonacci number of the indeces given
#' @section Quick Reference:
#' \preformatted{fib(2) outputs 1
#' fib(4) outputs 3
#' fib(c(1, 5, 8)) outputs [ 1, 5, 21 ]}
#' @export
#' @note This function is vectorized
#' @examples
#' fib(1)
#' fib(2)
#' fib(4)

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
#' @description Fast Fibonacci executes in constant time by using a formula instead of recursion
#' @param x the index of the desired fibonacci number
#' @return vector of the fibonacci number of the indeces given
#' @section Quick Reference:
#' \preformatted{fastfib(2) outputs 1
#' fastfib(4) outputs 3
#' fastfib(c(1, 5, 8)) outputs [ 1, 5, 21 ]}
#' @export
#' @note This function is vectorized
#' @examples
#' fastfib(1)
#' fastfib(2)
#' fastfib(4)
#' fastfib(c(1, 5, 8))

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
#' @description The Divisors Of Function calculates the divisors of integers
#' @param x the number to find the divisors of
#'
#' @return Returns a vector of divisors, must include 1 and the x itself if x was a single integer.
#' Returns a list of vectors if x was a vector of integers
#'
#' @section Quick Reference:
#' \preformatted{divisors_of(12) outputs [ 1, 2, 3, 4, 6, 12 ]
#' divisors_of(7) outputs  [ 1, 7 ]
#' divisors_of(c(10, 14, 5)) outputs list( [ 1, 2, 5, 10], [ 1, 2, 7, 14 ], [ 1, 5 ] )}
#'
#' @note This function is not exactly vectorized as passing a vector returns a list of vectors.
#'
#' @export
#' @examples
#' divisors_of(12)
#' divisors_of(7)
#' divisors_of(c(10, 14, 5))
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

#' Create Integer Object (S3)
#' @description Takes an integer to create an S3 object to save data about the integer inside the the integer object.
#' @param x integer to be created as an S3 object
#' @return object the integer S3 object with attributes
#' @section Quick Reference:
#' \preformatted{createIntegerObject(9) gives an object like
#' x {
#'   square: 81
#'   factors: 1, 3, 9
#' }}
#' @export
#'
#' @examples
#' createIntegerObject(9)
createIntegerObject <- function(x) {
  if(!is.numeric(x)) {
    stop("That is not a number")
  }
  if(!is.integer(x)) {
    as.integer(x)
  }
  object <- x
  attr(object, "square") <- 81
  attr(object, "factors") <- c(1, 3, 9)
  object
}

### Environments


#' Environ Test
#' @description Loops recursively through the environments until it reaches the Empty Environment prints the environment as it progresses
#' @return None
#' @export
#'
environTest <- function() {
  env <- environment()
  print(env)
  while (!identical(env, emptyenv())) {
    env <- parent.env(env)
    print(env)
  }
}

#' Function Getter
#' @description fget() returns a list of th
#' @param name the name of the function to search for
#' @param env the starting environment
#' @param inherits flag for allowing the function to look in parent environments also
#'
#' @return a function
#' @export
#'
#' @examples
#' fget("ls", environment())
fget <- function(name, env, inherits=TRUE) {
  result <- c()
  if(identical(env, emptyenv())) {
    stop("No funtion has that name in scope")
  }
  if(!exists(name, envir = env)) {
    result <- c(result, fget(name, parent.env(env)))
  } else {
    if(is.function(env[[name]])) {
      result <- c(result, attr(env, name))
    }
  }
  result
}

#### Non Standard Evaluation

### 1.

#' Deparse 2 (Wrapper for deparse in pryr)
#' @description Takes an expression and deparses it into a string. Unlike the original deparse, it will always be one string.
#' @param express
#'
#' @return a string representation of the expression
#' @section Quick Reference:
#' \preformatted{deparse2(1 + 8) outputs "1 + 8"
#' deparse2(1:10) outputs 1:10
#' deparse2(a + b + c + d + e + ... + z) outputs a + b + ... + z }
#' @export
#'
#' @examples
#' deparse2(1 + 8)
#' deparse2(1:12)
#' deparse2(square(17))
deparse2 <- function(express) {
  if(length(deparse(express)) > 1) {
    return(paste(deparse(express), sep = "", collapse = ''))
  }
  deparse(express)
}






