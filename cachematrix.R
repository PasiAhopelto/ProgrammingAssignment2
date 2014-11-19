## Overall description

## Write a short comment describing this function

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

## Write a short comment describing this function

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

a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
b <- makeCacheMatrix(a)
cacheSolve(b)
cacheSolve(b)
