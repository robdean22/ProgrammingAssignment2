## A function to cache the inverse of a matrix so that it won't have to be recalculated
## if needed again

## Creates a special matrix, which is really a list of functions to:
#   1. Set the value of the matrix
#   2. Get the value of the matrix
#   3. Set the value of the inverse
#   4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Calculates the inverse of the special matrix above, first checking to see whether it has
## already been calculated

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
