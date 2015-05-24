## [R] Programming: Assignment 2
## This function takes a matrix input (x) and outputs
## a matrix which is able to "cache" the inverse of (x).

## Function will make use of solve(x) since we can
## assume that the (x) is invertible, thus square.

makeCacheMatrix <- function(x = matrix()) {
        mca <- NULL
        set <- function(y){
                x <<- y
                mca <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mca <<- solve
        getinv <- function() mca
        list(set = set, get = get, setinv = setinv,
                getinv = getinv)
}


## The following function, cacheSolve, will look for 
## the cached value of the inverse before attempting
## a potentially costly calculation again:

cacheSolve <- function(x, ...) {
        invx <- x$getinv()
        if(!is.null(m)){
                message("Obtaining cached data...")
                return(m)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
        invx
}
