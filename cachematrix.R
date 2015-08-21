## Create a special "matrix" object that can cache its inverse.


## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## @param x A matrix
## @return A list containing four functions to set and get the value of the
##     matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

        setsolve <- function(solve) m <<- solve
        getsolve <- function() m

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.
## This function assumes that the matrix is always invertible.
## @param x a special matrix created with makeCacheMatrix
## @return The inverse of the matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
