##########################################################################
# Cache Matrix Utilities
##########################################################################
#
# Implements "cache matrix" datatype that upgrades a given matrix
# with getters and setters for the matrix itself, and for its inverse.
# Value of inverse is cached after first get, obviating future
# re-computation.
#
##########################################################################

## MAKECACHEMATRIX
#
# Creates special "cache matrix" datatype representing a matrix whose
# inverse is cached after the first time it is computed.  Matrix is
# represented via closure over a matrix and its inverse, via four getters
# setters: two for the matrix and two for its inverse.
#
# Parameter: x, the regular matrix to be upgraded to a cache matrix.
# To use, access any one of the getters or setters.
# 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getinv <- function() inv
    setinv <- function(i) inv <<- i
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## CACHESOLVE
#
# Computes inverse of special cache matrix (as created by MAKECACHEMATRIX
# above.)  But, if inverse has already created it, retrieves it from
# cache instead of computing anew.
#
# Parameter: x, the special cache matrix to be inverted.
#
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        inv
    } else {
        m <- x$get()
        mp <- solve(m, ...)
        x$setinv(mp)
        mp
    }
}
