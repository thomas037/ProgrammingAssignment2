## Matrix inversion is usually a costly computation and their may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.
## The following pair of functions cache the inverse of a matrix.

## makeCacheMatrix(): This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Ans <- NULL
        set <- function(y) {
                x <<- y
                Ans <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) Ans <<- solve
        getSolve <- function() Ans
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" x returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        Ans <- x$getSolve()
        if(!is.null(Ans)) {
                message("getting cached data")
                return(Ans)
        }
        data <- x$get()
        Ans <- solve(data, ...)
        x$setSolve(Ans)
        Ans
}
