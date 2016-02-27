## This function creates a cache matrix object which calculates the inevrse of invertible
## matrix once and can be used repatedly by getting the inverse value from the cache.
## 
## Usage:
## x <- matrix(1:4, nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(x)
## cacheSolve(cacheMatrix)
##
## Functions:
## set() # Change the matrix being cached.
## get() # Returns the matrix being cached.
##
## setInverse() # contains cached inverse value
## getInverse() # Used to get the cached inverse value

## Create a cacheMatrix object for an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInverse) inverse <<- matrixInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
