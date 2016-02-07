## Compute the inverse of a matrix usually is a costly compuntation and there may be some benefit to caching the inverse rather than computing it repeatdly. 
## The following functions are used to create a matrix and caches its inverse.  


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
}
