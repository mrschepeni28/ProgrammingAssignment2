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
  
  ## Initialize Inverse of matrix to NULL
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  ## set Inverse of Matrix using solve
  setinv = function(solve) inv <<- solve 
  ## Get inverse of matrix
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## If the inverse has already been calculated (and the matrix has not changed),
  ## then the cachesolve should retrieve the inverse from the cache.
  ## Computing the inverse of a square matrix can be done with the solve function in R.
  ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  
  ## Get Inverse
  m <- x$getinv()
  ## If inverse exists then get it
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  ## If inverse doesnt exist, then compute using solve
  compute <- x$get()
  m <- solve(compute, ...)
  x$setinv(m)
  m
}
