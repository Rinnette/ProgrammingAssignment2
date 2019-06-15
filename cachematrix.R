## These functions get the inverse of a matrix.  The inverse is cached so that if for some  
## reason it is required repeatedly it will not have to be computed over and over again.


## This function creates a special "matrix" object that caches its inverse.  It returns a list
## of 4 functions that allow you to access the object and the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {                ## set the value of the "matrix" object
        x <<- y
        inv <<- NULL
    }
    get <- function() x                 ## gets the value of the object
    setinv <- function(inv) i <<- inv   ## sets the inverse of the "matrix"
    getinv <- function() i              ## gets the inverse of the "matrix"
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of the special "matrix" object returned by the 
## makeCacheMatrix function.  If the inverse has already been calculated and cached, cacheSolve 
## retrieves the inverse, else it calculates the inverse.

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
