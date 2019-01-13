## makeCacheMatrix creates an R object that stores a
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## CacheSolve retrieves the inverted matrix that is stored
## in the makeCacheMatrix object's environment.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <-x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}
