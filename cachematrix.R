## The two functions below are a pair of functions which implement a matrix that
## when calculation of its inverse calculated once, it cached for the next times it called.


## this function create the special matrix contain get set functions of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse_m) inv <<- inverse_m
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## caclute the inverse of a matrix unless it already cached (not null).

cacheSolve <- function(x, ...) {
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
