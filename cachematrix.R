## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    cachedInverse <- NULL
    
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    get <- function() { x }
    
    getInverse <- function() { cachedInverse }
    
    setInverse <- function( inverse ) {
        cachedInverse <<- inverse
    }
    
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## cacheSolve computes the inverse of the special matrix created using
## makeCacheMatrix; If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve will return the
## cached value; if x is a regular matrix, then simply return the inverse

cacheSolve <- function(x, ...) {
    
    if (class(x)=="list"
                && !is.null(x[["get"]])
                && !is.null(x[["set"]])
                && !is.null(x[["getInverse"]])
                && !is.null(x[["setInverse"]])                                       
              ) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
            message("x is a special matrix: getting cached data")
            return(inverse)
        }
        
        message("x is a special matrix with no cached inverse: computing and caching inverse")
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        return(inverse)
    }
    
    else {
        message("x is not a special type of matrix created with makeCacheMatrix; attempting to calculate inverse directly")
        return(solve(x));
    }
}
