## These two functions provide a mechanism for reducing the computational
## cost of calculating the inverse of a matrix. The functions 'cache' the 
## result of the inverse the first time cacheSolve() is called.
## This dramatically improves execution time for all subsequent calls to 
## cachSolve as the inverse will not be recalcuated, it will be retrieved
## from the cache. These functions make use of the <<- to assign the cached
## result to the object in the calling environment.
## 
## Make Cached Matrices
##
## Function to create matrix caching functionality to a list object so a 
## matrix and its inverse can be stored and retrieve. Used with cacheSolve().
##
## Usage
## makeCacheMatrix(x = matrix())
##
## Arguments
## x    An R matrix object (Default)
##
## Examples
## m <- makeCacheMatrix()   # Create the cacheable matrix
## m$set(matrix(1:4,2,2))   # Set the matrix
## cacheSolve(m)            # Solve and return the inverse, caching if required
## 

makeCacheMatrix <- function(x = matrix()) 
{
    ## Variable to store the inverse of the matrix. Set to NULL by default.
    i <- NULL
    
    ## Function to assign the Matrix
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    
    # Function to retrieve the Matrix
    get <- function()
    {
        x
    }
    
    # Function to set the Inverse of the Matrix
    setinverse <- function(inverse)
    {
        i <<- inverse
    }
    
    # Function to retrieve the Inverse of the Matrix
    getinverse <- function()
    {
        i
    }
    
    # Return a list of functions that operate over the Matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cache Solve (inverse matrices)
##
## Function to solve the inverse of a matrix, cache if the first time, then
## return the result as a matrix object. Subsequent calls will not calculate
## the inverse but instead return the result from the cache
## Used with makeCacheMatrix().
##
## Usage
## cacheSolve(x = matrix(), ...)
##
## Arguments
## x    An R matrix object.

cacheSolve <- function(x = matrix(), ...) 
{
    ## Retrieve the stored inverse
    i <- x$getinverse()

    ## Test if the cache has been previously calculated
    if(!is.null(i)) 
    {
        ## We have the inverse in i, notify user
        message("getting cached data")
    }
    else
    {
        ## Need to create the inverse
        data <- x$get()         ## Retrieve the matrix
        i <- solve(data, ...)   ## Calculate the inverse
        x$setinverse(i)         ## Store the result in the cache  
    }

    ## Return a matrix that is the inverse of 'x'
    return(i)
}
