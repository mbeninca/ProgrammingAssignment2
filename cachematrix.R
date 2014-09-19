## Set of functions to speed matrix inverse calculations,
## cacing the inverse to calculate it just once. 

## makeCacheMatrix - function to cache a matrix.
## Requires a matrix as input and returns a list os functions
## to save and retrieve the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # set function caches a matrix in parent environment
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }        
        # get function retrives a matrix from cache
        get <- function() x
        
        # setinv function caches the matrix inverse
        setinv <- function(inv) inverse <<- inv
        
        # getinv function retrieves the matrix inverse
        getinv <- function() inverse
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve - function to calculate matrix inverse or retrive
## the inverse from cache if it was already stored.
## Requires a cached matrix built from makeCacheMatrix function,
## as input. Returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()        
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }        
        # Calculate 'x' inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
