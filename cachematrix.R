## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    # variable where we store the cached inverse
    inv <- NULL
    
    # sets the matrix content of our cached inverse matrix
    # also invalidates the cached inverse
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    # retrieves the matrix content of our cached inverse matrix
    get <- function() x
    
    # stores the computed inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # retrieves the cached inverse
    getInverse <- function() inv
        
    # offers the four functions above
    list(
        set = set, get = get,
        setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...)
{
    # check whether the matrix has an inverse already cached
    inv <- x$getInverse()
    
    # if it has
    if (!is.null(inv))
    {
        # return it, and notify the caller via message 
        message("Getting cached inverse")
        return(inv)
    }

    # otherwise, compute the inverse
    # passing any extra parameters to the solve function
    # Caveat, and potential improvement: calling cacheSolve with *different*
    # extra parameters will not invalidate the cached inverse
    message("Computing inverse")
    inv <- solve(x$get(), ...)

    # cache the result
    x$setInverse(inv)
    
    # return the inverse
    inv
}
