###############################################################
## A pair of functions that cache the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible
###############################################################

## This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse matrix
    setinverse <- function(xinv) m <<- xinv
    
    # Get the value of the inverse matrix
    getinverse <- function() m

    # Return a list containing functions to set and get the 
    ## values of the matrix and its inverse
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object
## returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    # Get the inverse of matrix 'x'
    m <- x$getinverse()
    
    ## Check if the inverse has already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        
        ## Return the inverse of matrix 'x'
        return(m)
    }
    
    ## Calculate the inverse of matrix 'x' and set the value of it 
    ## in the cache via the setinverse function
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## Return the inverse of matrix 'x'
    return(m)
}