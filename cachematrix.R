## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # set function caches a matrix in parent environment:
        set <- function(y){
                x <<- y
                m <<- NULL
        }        
        # get function retrives a matrix from cache
        get <- function() x
        
        # setinv function caches in matrix inverse
        setinv <- function(inv) inverse <<- inv
        
        # getinv function retrieves the matrix inverse
        getinv <- function() inverse
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

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
