##  Matrix inversion is usually a costly computation and there may be some
##  benefit to caching the inverse of a matrix rather than computing it
##  repeatedly. The pair of functions implemented herein caches the inverse
##  of a matrix and return it in case additional inverse calculation is needed.


## `makeCacheMatrix` creates a special "matrix" object
##   that can cache its inverse.

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


##  `cacheSolve` computes the inverse of the special "matrix" 
##  returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed),
##  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
