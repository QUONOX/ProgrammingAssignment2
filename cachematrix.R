## Computing the inverse of a square matrix using caching to attain computational efficiencies

## Function 'makeCacheMatrix' creates a special matrix that can cache it's inverse. Includes functions:
## Set values of a matrix - Function 'set'
## Get values of a matrix - Function 'get'
## Set the inverse of a matrix - Function 'setinverse'
## Get the inverse of a matrix - Function 'getinverse'

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## Function 'Set'
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Function 'Get'
  get <- function() x
  
  ## Function 'setinverse'
  setinverse <- function(inverse) m <<- inverse
  
  ## Function 'getinverse'
  getinverse <- function() m
  
  ## Return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function 'cacheSolve' computes the inverse of the matrix returned by function 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  ## Return inverse of matrix 'x'  from the cache if already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## Returns inverse of matrix 'x' if not already calculated
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
