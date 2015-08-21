# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion 
# that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse 
# of a matrix.

# Write the following functions:

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example, if X
# is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix(x)
# Declare the cache Matric functions and return the list of available object function
# Parameters :
#   x : a square invertible matrix
# Retruns :
#   List of functions 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # set()
    # Set the Matrix in cache and reset reverse Matrix
    # Parameters :
    #   y : Matrix to cache
    set <- function(y) {
        # cache the matrix
        x <<- y
        # reset reverse matrix
        m <<- NULL
    }
    
    # get()
    # Get the Matrix 
    # Returns: 
    #   The cached matrix
    get <- function() x
    
    # setreverse(reverse)
    # set reserve matrix in cache
    # Parameters :
    #   reverse : reverse matrix to cache
    setreverse <- function(reverse) m <<- reverse
    
    # getreverse()
    # get reverse matrix
    # Returns
    #   The cached reversed matrix
    getreverse <- function() m
    
    # Return list functions
    list(set = set, get = get,
         setreverse = setreverse,
         getreverse = getreverse)
}


# cacheSolve(x)
# Creates a reverse matrix and cache it
# Parameters
#   x: makeCacheMatrix object
#   ... : parameters given to the solve() function applied on the matrix
# Returns
#   the reserve matrix of makeCacheMatrix
cacheSolve <- function(x, ...) {
    # Return the cache reverse matrix or NULL if it hasn't been calculated
    m <- x$getreverse()
    # check if the reverse matric is not null
    if(!is.null(m)) {
        # if reverse matrix is cached, 
        message("getting cached data")
        return(m)
    }
    # get the matrix
    data <- x$get()
    # get the reverse matrix
    m <- solve(data, ...)
    # store the reverse matrix
    x$setreverse(m)
    # return the reverse Matrix
    m
}
