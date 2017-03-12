## These functions will get the inverse of a matrix if it has been cached and calculate it if it has not

## This sets the value of a matrix, gets the value of a matrix, sets the value of its inverse,
## and gets the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This computes the inverse of the matrix, unless the matrix has not changed and the inverse
## has been calculated, in which cases it retrieves it from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    n<-x$get
    if(!is.null(m) & n==x$get) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
