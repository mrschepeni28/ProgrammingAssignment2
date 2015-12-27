## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly
## These pair of functions cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  ## This matrix is used as the input to cacheSolve()
  ## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
  ## set the Matrix
  ## get the Matrix
  ## set Inverse of the Matrix
  ## get Inverse of the Matrix
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(solve) inv <<- solve 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then the cachesolve should retrieve the inverse from the cache.
  ## Computing the inverse of a square matrix can be done with the solve function in R.
  ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  compute <- x$get()
  m <- solve(compute, ...)
  x$setinv(m)
  m
}
