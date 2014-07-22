## makeCacheMatrix and cacheSolve are functions designed to take calculate 
## the inverse of a matrix and store the inverse matrix for repeated use
## Rory Waisman, 2014

## store the matrix provided by the user as an argument in the function call 
## replace the matrix with a matrix provided as an argument in a call to
## the set() function
## store the inverse matrix which is provided as an argument in a call to the 
## setinv() function
## get() returns the original matrix 
## getinv() returns the inverse matrix that has been stored


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## checks the cache for an inverse of the matrix and return it if it exists
## if there is nothing in the cache, calculate the inverse and 
## store it by a call to the setinv() functiomn of makeCacheMatrix
## and return the inverse matrix

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
