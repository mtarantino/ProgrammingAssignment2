#############################################
# makeCacheMatrix(x)
# Declare the cache Matric functions and return the list of available object function
# Parameters :
#   x : a square invertible matrix
# Retruns :
#   List of functions 
#############################################
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

#############################################
# cacheSolve(x)
# Creates a reverse matrix and cache it
# Parameters
#   x: makeCacheMatrix object
#   ... : parameters given to the solve() function applied on the matrix
# Returns
#   the reserve matrix of makeCacheMatrix
#############################################
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
