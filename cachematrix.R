## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly. In this R file, there are two function that deal with that.

## This function creates a special "matrix" object that can cache its inverse, 
## which is really a list containing a function to
## set : set the value of the matrix, this function gets the same arguments of "matrix()" function
## get : get the value of the matrix
## setinv : set the value of the inverse of the matrix
## getinv : get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(...) {
        x <<- matrix(...)
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
