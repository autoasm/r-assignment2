
# This R program illustrates how to use cache mechanism to improve performance
# of computation, some results of time-consuming computation can be stored in
# a global variable which can be accessible to many dependent modules.

# Run below commands to verify these functions.

## source("cachematrix.R")
## c=rbind(c(1, -1/4), c(-1/4, 1))
## data <- makeCacheMatrix(c)
## cacheSolve(data)
## cacheSolve(data)


# Make a cache matrix. Callers can call getInverse() function to get cached
# inverse of matrix, the setInverse() function allows users to update cached data.
makeCacheMatrix <- function(x = matrix()) {

    # Initialize m
    m <- NULL

    # Reset x and m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Get data x
    get <- function() x

    # Set m
    setInverse <- function(inverse) m <<- inverse

    # Get m
    getInverse <- function() m

    # Returns functions
    list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
}


# This function shows how to use cache matrix. It get inverse from cache, and it
# would also update the cache if cached object is not available.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'

    # Get inverse from cache
    m <- x$getInverse()

    # Return the m if it is not a null
    if(!is.null(m)) {
        message("getting cached data")
        return (m)
    }

    # Get data
    data <-x$get()

    # Compute the inverse of the matrix
    m <- solve(data) %*% data

    x$setInverse(m)
    m
}
