## makeCacheMatrix and cacheSolve together cache the inverse of a matrix 
## appaers to be some benefit to cache inverse rather than compute repeatedly

## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If inverse has already been calculated, cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        else {
                inv <- solv(x$get())
                x$setinv(inv)
                return(inv)
        }
}
## Return a matrix that is the inverse of 'x'

