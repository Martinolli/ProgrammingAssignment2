## Put comments here that give an overall description of what your
## functions do

## This function This function creates a special "matrix" object
## that can cache its inverse.
## The matrix must be squared and invertible one.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
                  setsolve = setsolve,
                  getsolve = getsolve)
}


##  This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##  If the inverse has already been calculated (and the matrix has not changed).
##  This function works just with square matrix and invertible one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
