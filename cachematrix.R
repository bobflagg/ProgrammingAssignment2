## This script defines a special matrix object and a function which works with
## the special object to provide an "efficient" version of solve for finding 
## the inverse of a matrix and caching the result to avoid recomputing the it 
## when the original matrix has not changed since the last time the inverse 
## was computed.
##
## Example:
## x <- matrix(c(1,0,3,2,2,4,3,2,1),ncol=3)
## x.special = makeCacheMatrix(x)
## cacheSolve(x.special)
## x %*%  x.special$getinverse()
## x.special$getinverse() %*%  x

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
      x <<- y
      x_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x_inv <<- inv
    getinverse <- function() x_inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## This function is an "efficient" version of solve for finding the inverse of a matrix,
## which uses the special "matrix" object defined above that can cache the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinverse()
    if(!is.null(x_inv)) {
      message("getting cached data")
      return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinverse(x_inv)
    x_inv
}
