## This program
## - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## makeCacheMatrix:
## - Desc: create a special "Matrix" object that can cache its inverse.
## - Input: a matrix
## - Output: a special "matrix" object containing the original matrix, a cache and a set of methods to set/get the matrix and cache.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(msolve) s <<- msolve
  getSolve <- function() s
  list (set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve
## - Desc: Compute the inverse of the special "matrix" object. Retrieve cached value when possible
## - Input: the special "matrix" object
## - Output: the inverse of the matrix

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

c=rbind(c(1, -1/4), c(-1/4, 1))  

t=makeCacheMatrix(c)
cacheSolve(t)
cacheSolve(t)
