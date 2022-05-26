## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below functions allow for the caching of the inverse of a matrix instead of
## recalculating the inverse everytime it is required


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
  }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve will return the inverse of a matrix stored in makeCacheMatrix
## the inverse is calculated when first called and then "cached." On subsequent calls the cached value is returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
