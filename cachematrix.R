## Assignment: Caching the Inverse of a Matrix
## A pair of functions that cache the inverse of a matrix instead of repeatedly computing the inverse so as to cut on costly computations.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
   invMatrix <- NULL 
   
    set <- function(y) {
        x <<- y
       invMatrix <<- NULL
    }
   
    get <- function() x
    setinv <- function(solve) invMatrix <<- solve(x)
    getinv <- function() invMatrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinv()
    
    ## check if already inverted and return the inverted matrix
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    ## if not already inverted, invert and return the inverted matrix
    data <- x$getinv()
    invMatrix <- solve(data, ...)
    x$setinv(invMatrix)
    invMatrix 
}
