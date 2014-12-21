## We define a custom object with functions for setting and getting
## the value of matrix and it's inverse.

## Custom function that stores a matrix and it's inverse which is solved
## with another function. Upon setting a new value of matrix, the inverse
## is reset.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if inverse matrix is already cached. If it is it
## returns the cache, otherwise it solves the matrix for the inverse
## and caches the solution.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached inverse matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
        ## Return a matrix that is the inverse of 'x'
}
