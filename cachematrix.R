## Programming in R - assignment 2
## mecblux - 22 March 2015
##
## These functions allow to store the result of the data intensive
## computation of inversing a matrix, so that, once the inverse
## has been computed, it can be taken from the cache instead
## of recomputing it

## The function makeCacheMatrix creates a special matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse
    inv <- NULL
    ## defining the four object related functions 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    ## creating the special matrix object with four functions listed
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve computes the inverse of a special matrix
## returned by makeCacheMatrix, or returns the inverse that has 
## been cached before by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## inspect content of cache
    if(!is.null(inv)) { 
        ## if the inverse already exists it is taken from cache
        message("getting cached data")
        return(inv)
    }
    ## if the inverse is not cached yet, it is computed...
    data <- x$get()
    inv <- solve(data, ...)
    ## ... and then cached
    x$setinv(inv)
    inv
}
