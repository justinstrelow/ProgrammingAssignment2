## Function pair for inversing a matrix and then storing it in cache for
## easy retrieval

## This does no calculations.  It is simply meant to set and get both the
## matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Takes in a matrix and then inverts it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #grabbing the matrix from the other function
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
