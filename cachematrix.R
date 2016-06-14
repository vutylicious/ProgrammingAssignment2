## The calculation of the inverse of a matrix could be computationally costly.  
## These two functions allow for caching of a matrix's inverse so that the 
## calculation only needs to happen once even if you are using the inverse 
## multiple times.

## The makeCacheMatrix function takes one parameter, a matrix.  It essentially
## creates an "object", returning a list with four functions:
##   get() -- returns the original matrix
##   set(x) -- changes the matrix stored within the object
##   getInverse() -- returns the inverse of the matrix
##   setInverse() -- allows for storing the inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The cacheSolve function takes as its first parameter the object returned by
## makeCacheMatrix.  Additional parameters passed to the solve function will 
## also be accepted.  The cacheSolve function returns the inverse of the matrix
## stored in the object, with the benefit of caching the inverse so repeated
## calculations are unnecessary.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
