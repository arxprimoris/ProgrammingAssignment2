## Assignment 2 for Coursera R Programming Course
##  Helper functions for solving and cacheing the inverse of a matrix

## Recieves as input a matrix
## Returns a list of functions that set/get the cached matrix and
##   set/get the cached inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inversion) inv <<- inversion
    getInv <- function() inv
    
    return (list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## Recieves as input a makeCacheMatrix object and any other parameters to be 
##   passed to the solve() function.
## Returns an inverse of the input matrix. If a cache for for the inverse exists
##   it is returned. If a cache of the inverse does not exist, it is computed.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    
    if(!is.null(inv)) {
        message("Returning cached inverse...")
        return(inv)
    }
    
    message("Computing inverse...")
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInv(inv)
    
    return(inv)
}
