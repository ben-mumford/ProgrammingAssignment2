## A pair of functions to produce a cache-able matrix and to calculate and store the inverse

## Creates a special "matrix" with an additional inverse field

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
