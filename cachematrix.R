## The cachematrix.R file contains two functions
## 1. makeCacheMatric() and 
## 2. cacheSolve() 
## The first function creates an R object that 
## stores a matrix and the inverse of the matirx
## The second function cacheSolve() function retrieves the 
## inverse of the matric from the cached value that is stored
## in the universe of the makeCacheMatrix () Environment


## The function makeCacheMatrix initializes two (2) objects
## they are x and invmtrx
## and then it establishes functions called 
## get() 
## set() 
## setinvrs()
## getinvrs()
## within set() <<- form of assignment operator is used which assigns value 
##    on the right side of the operator to an object in the parent environment 
##     named by the object on the left side of the operator 
## get() functions uses the concept of lexical scoping features in R 
##      x is not defined within get(), it is being retrieved from the parent environment
## setinvrs() assigns the input argument to the value of invmtrx in the parent environment
## and then getinvrs() defines get process for the inverse of the matrix
## Each of these functions is assigned as an element within a list(), and is returned to the parent environment

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


## cacheSolve() is the function which uses the solve() function to get the inverse of the matrix
## It first checks whether a new matrix has been passed, if not , the cached inverse of the matrix is returned
## otherwise, it calculates the inverse and returns it 
## !is.null(invmtrx) helps to check whether a new inverse needs to be calculated or it can be returned from the cache
## this is depending on the statement invmtrx <<- NULL

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
