rm(list = ls())
## Matrix inversion involves a tad complicated computation, which might delay the time-efficiency
## Therefore, in an effort to explore the opportunity to avoid the repeated calculation of same values,
## the below two functions can be used to create a cache of the computation, there by saving run-time.

## makeCacheMatrix function creates a list containing functions to:
## - To set the value of the matrix
## - To get the value of the matrix
## - To set the value of inverse of the matrix
## - To get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse_ <- NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_ <<- inverse
  getinverse <- function() inverse_
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## The below function returns the inverse of the matrix given as an input.
## In case it has been already passed through the above function, it would've loaded into the cache,
## which wil be checked, if already computed then it will retrive from cache and skips the calculation.
## In case, it has not been calculated earlier, then it does and sets the value into cache, via set function.

cacheSolve <- function(x, ...) {
  
    inverse_ <- x$getinverse()
    if(!is.null(inverse_)) {
      message("Retrieving the cached data")
      return(inverse_)
    }
    data <- x$get()
    inverse_ <- solve(data, ...)
    x$setinverse(inverse_)
    inverse_
  }
