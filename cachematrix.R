## These functions create a matrix and return an inversion of the matrix.
## If the inverted matrix has already been created before, cacheSolve will
## return the cached answer instead of redoing the calculations.

## This handles our setup and stores cached information.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) m <<- solve
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## This calculates the inversion of the matrix unless a cached version exists.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
  
}
