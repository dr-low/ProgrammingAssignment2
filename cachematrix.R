## Week 3 R Programming Assignment
##
## This file contains two functions
## - makeCacheMatrix caches the inverse of a matrix
## - cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

## makeCacheMatrix
##   cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list( set = set, get = get, setinv = setinv, getinv = getinv )
}


## cacheSolve
##   compute the inverse of a matrix by either retrieving a cached inverse
##   or calculating (and storing) an inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    s <- x$getinv()
    if (!is.null(s)) {
        message( "getting cached data" )
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}

