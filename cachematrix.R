## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The follwoing 2 functions can create a special Matrix object that cache the inverse 
## of the matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Retrieve the inverse 
  inv <- x$getinverse()
  
  # if the inverse does not exist, compute it then set it 
  # if it exists, just print a message
  if ( is.null(inv) ) {
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
  } else message("Retrieving cached data")
  
  inv
}