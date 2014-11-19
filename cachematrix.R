## Functions 'makeCacheMatrix' and 'cacheSolve' return a matrix than can cache 
## its inverse, and calculate matrixes inverse while utilizing possibly cached 
## value.

## makeCacheMatrix takes a matrix as a parameter and returns a list of four functions:
## - set(matrix) sets the matrix to given value 
## - get returns the matrix
## - setInverse(matrix) sets matrices inverse
## - getInverse returns cached inverse, or null if it hasn't been set.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve takes list returned by makeCacheMatrix, and returns its inverse. It 
## uses cached inverse, if available. If not, it calculates it and sets it to 
## "matrix" that was given as a parameter.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    print(inverse)
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
