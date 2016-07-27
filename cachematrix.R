#Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly.
# This file consists of a pair of functions which
#can be used to cache the inverse of a matrix.

#This function creates a special "matrix" object 
# that can cache its inverse.
# Input should be a invertible matrix
# Returns list of methods to access the attributes
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  ## Setter for matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Getter for matrix
  get <- function()
    x
  ## Setter for inverse
  setinv <- function(inv) {
    m <<- inv
  }
  ## Getter for inverse
  getinv <- function() {
    m
  }
  ##List of attributes
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}

#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix. If the inverse has already
#been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
    ## Check if cache has the inv value
    message("getting cached data")
  } else{
    data <- x$get() ## Get the original matrix
    m <- solve(data) ## Find the inverse
    x$setinv(m) ## Set in cache
  }
  ## Retrun value
  m
}