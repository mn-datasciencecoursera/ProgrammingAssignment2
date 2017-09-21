## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we
## will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
## For this assignment, assume that the matrix supplied is always invertible.

## Computing the inverse of a square matrix can be done with the solve function in R. For example, 
## if X is a square invertible matrix, then solve(X) returns its inverse.

## the <<- operator can be used to assign a value to an object in an environment that is different 
## from the current environment. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) s <<- solved
    getsolved <- function() s
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolved()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolved(s)
    s
}
