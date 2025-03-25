## The following functions implement a caching system for computing the inverse of a matrix
##
## example to validate: 
##   t <- makeCacheMatrix(matrix(c(2, 3, 1, 4), nrow = 2, ncol = 2))
##   cacheSolve(t)
##
## expected response
##       [,1] [,2]
## [1,]  0.8 -0.2
## [2,] -0.6  0.4

## creates a cachable matrix object
makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse
        v <- NULL

        # reset the value of the matrix and the cached object
        set <- function(y) {
                x <<- y
                v <<- NULL
        }

        # get operation
        get <- function() x

        # get Inverse operation to return the inverse matrix from the cache
        getInverse <- function() v

        # set Inverse operation tp update and cache the matrix
        setInverse <- function(inv) v <<- inv

        # return a list of the get/set functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        # get the inverse from the cache
        v <- x$getInverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }

        # compute the inverse and cache it
        m <- x$get()
        v <- solve(m, ...)
        x$setInverse(v)

        v
}
