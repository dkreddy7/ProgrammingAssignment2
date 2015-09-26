## The makeCacheMatrix function creates a matrix and cacheSolve
## function is used to inverse it.  It has already been calculated
## then it retrieves from cache.

## This function creates a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(mean) {
    m <<- mean
  }
  getinverse <- function() {
    m
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix
## returned by makecacheMatrix.  If inverse has already been
## calculated, it retrieves from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## solve is used to calculate the inverse of a square matrix
  m <- solve(data)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
