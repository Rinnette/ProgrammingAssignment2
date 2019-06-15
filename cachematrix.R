## These functions cache the inverse of a matrix so that if for some reason it is required 
## repeatedly it will not have to computed over and over again.

## This function creates a special matrix "object" that caches its inverse.  It returns a list
## of 4 functions that allow you to access the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This functions calculates the inverse of the special matrix "object" returned by the 
## makeCacheMatrix function.  If the inverse has already been calculated, cacheSolve retrieves
## the inverse from the cache, else it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
