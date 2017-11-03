## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invmtrx <- NULL
    set <- function(y) {
        x <<- y
        invmtrx <<- NULL
    }
    get <- function() x
    setinvrs <- function(inverse) invmtrx <<- inverse
    getinvrs <- function() invmtrx
    list(set = set, 
         get = get,
         setinvrs = setinvrs,
         getinvrs = getinvrs)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmtrx <- x$getinvrs()
    if(!is.null(invmtrx)) {
        message("getting cached data")
        return(invmtrx)
    }
    data <- x$get()
    invmtrx <- solve(data, ...)
    x$setinvrs(invmtrx)
    invmtrx
}
