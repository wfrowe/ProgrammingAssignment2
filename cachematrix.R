## WFR R Programming Assignment 2

## Cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Set the value of the matrix
    inv <- NULL    ## Start the inverse at null
    set <- function(y) {
        x <<- y  ## when set is called need to change the matrix to the new one
        inv <<- NULL ##null out again
    }
    
    ## get the value of the matrix
    get <-function() x
    
    ## Set the value of the inverse
    setinv <-function(inverse) inv <<- inverse  
    
    ## get the value of the inverse
    getinv <- function() inv
    
    ## Create the object so cacheSolve can run onit
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## make the inverse from the first function
    inv <- x$getinv()
    
    ## if the inverse is already calculated because the matrix hasn't changed, return the cache
    if(!is.null(inv)) {
        message("Getting Cached Data")
        return(inv)
    }
    
    ## If we've gotten this far...
    ## Define what 'data' is so we can do stuff to it (i.e. solve the matrix in the next line)
    data <- x$get()
    inv <- solve(data, ...)
    
    ## set the new inverse so makeCacheMatrix is up to data
    x$setinv(inv)
    
    ##return the inverse
    inv
}