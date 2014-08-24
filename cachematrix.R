## These functions are used to create a special object that stores a 
## numeric matrix and cache's its inverse. 
## Useful when have potentially time-consuming computations. 

## This function creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setinv <- function(x_i) xInv <<- x_i
        getinv <- function() xInv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the special matrix created
## with the 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
        ## Return a matrix 'x_inv', that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv
}
