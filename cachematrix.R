## This program
## - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## makeCacheMatrix:
## - Desc: create a special "Matrix" object that can cache its inverse.
## - Input: 
## - Output: a special "matrix" object:

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setSolve <- function(msolve) s <<- msolve
    getSolve <- function() s
    list (set = set, get = get, setmean = setmean, getmean = getmean)
}


## cacheSolve
## - Desc: 
## - Input:
## - Output: 

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if (!is.null(s)) {
      message("getting cached data")
      return (s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
