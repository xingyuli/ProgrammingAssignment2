## Create an enhanced version of given matrix 'x'. The returned version is able
## to remember the previous inverse matrix of 'x'. Note that, matrix changes
## caused by calling 'set' will clean the remembered inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setinversed <- function(i) inversed <<- i
    getinversed <- function() inversed
    list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)
}


## Return a matrix that is the inverse of 'x'(which is produced by
## 'makeCacheMatrix'). The computation will only occur if the inverse matrix is
## not computed yet.

cacheSolve <- function(x, ...) {
    i <- x$getinversed()
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinversed(i)
    i
}
