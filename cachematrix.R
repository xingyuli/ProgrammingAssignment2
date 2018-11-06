## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinversed()
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinversed(i)
    i
}
