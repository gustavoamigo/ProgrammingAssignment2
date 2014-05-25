## The two functions below are used to cache the costly operation of matrix
## inversion. The first function makeCacheMatrix is used to create a special
## type of "matrix" object that can cache the inverse of the matrix. The
## second function cacheSolve is used to calculate the inversion of the matrix
## object created with makeCacheMatrix. It will return a cached value if the 
## inversion was calculated before. 

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## The object created has the following functions:
##   * setsolve, getsolve - gets and sets the result of the solve function
##   * get, set - gets and sets the original matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
