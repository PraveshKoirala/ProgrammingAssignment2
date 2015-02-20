##  Create two functions makeCacheMatrix and cacheSolve to solve and cache
##  the inverse of a matrix.
##  

##  A creator function that creates and returns a list of 
##  functions that provide facilities to set or retrieve a matrix
##  and set, retrieve and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) inverse <<- inv
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  A function to query the list and return inverse, if inverse is in cache
##  else calculate, cache and return the inverse. The cache can then be accessed
##  in subsequent calls

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)){
        message("returning inverse from cache")
        return (inverse)
    }
    
    # inverse is not in cache, calculate it and set it in cache.
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
    
}
