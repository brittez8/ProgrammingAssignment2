## Creating two functions; the first caches the inverse of a matrix and
## the second computes either retrieves the matrix inverse if it has already
## been calculated or computes it if the inverse in not found in the cache

## Function creates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function either retrieves the matrix inverse if it has already been
## calculated or computes it if the inverse in not found in the cache
## (Return a matrix that is the inverse of 'x')

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}
