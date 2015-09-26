
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following functions cache the inverse of a matrix: 

# makeCacheMatrix creates a list containing a function to
# 1. setMatrix -set the value of the matrix
# 2. getMatrix - get  the value of the matrix
# 3. setInverse - set the value of inverse of the matrix
# 4. getInverse -get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL      #initial condition
  setMatrix <- function(new_value) {
    x <<- new_value
    cache <<- NULL
  }
  getMatrix <- function() {
    x
  }
  setInverse <- function(solve) {
    cache <<- solve
  }
  getInverse <- function() {
    cache
  }
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. Otherwise, it computes the inverse, sets the value in the cache through
# setinverse function.

# This function assumes that the matrix is always invertible.It returns the inverse of makeCacheMatrix


cacheSolve <- function(m, ...) {
  inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$getMatrix()
  inverse <- solve(data)
  m$setInverse(inverse)
  inverse
}
