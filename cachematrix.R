# R Programming - Assignment 2: Lexical Scoping
# Written by Nicholas Heng Loong Wong
# 22 February, 2015

## 'makeCacheMatrix' is used to create a special "matrix" object with functions
## having the ability to cache the matrix's inverse. A matrix object is first
## created by calling 'makeCacheMatrix'. 'cacheSolve' can then be called to
## calculate the matrix's inverse or retrieve it from the object's cache.
## E.g. on the first run, 'cacheSolve' will calculate the matrix objects inverse
## and save it in the cache. On subsequent calls to 'cacheSolve', the matrix
## inverse already stored in the cache will be retrieved and returned.

## Creates a special "matrix" object 'x' with the ability to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 # Initialise inverse
    
    # Sets the value of the matrix object
    set <- function(y) {
        x <<- y                 # Value assignment across environments
        inv <<- NULL            # Initialise inverse
    }
    
    # Retrieves the value of the matrix object
    get <- function() x
    
    # Sets the inverse of the matrix object
    setInv <- function(inverse) inv <<- inverse
    
    # Retrieves the inverse of the matrix object
    getInv <- function() inv
    
    # Return the list of matrix object function commands
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Calculates the inverse of a "matrix" object 'x', or retrieves the inverse if
## it already exists in the object's cache. Returns the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Attempt to retrieve the matrix object's inverse from cache
    inv <- x$getInv()
    
    # If inverse had been previously calculated and stored, retrieve it from
    # cache and return
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the matrix object's inverse
    data <- x$get()             # Get the value of the matrix object
    inv <- solve(data, ...)     # Calculate its inverse
    x$setInv(inv)               # Store the calculated inverse in the cache
    inv                         # Return the calculated inverse
}
