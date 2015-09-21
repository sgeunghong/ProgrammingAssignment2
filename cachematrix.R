## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function(y) {
    x <<- y
    cachedinverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) cachedinverse <<- solve
  getsolve <- function() cachedinverse
  list(set = set, get = get,       setsolve = setsolve,       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedinverse <- x$getsolve()
  if(!is.null(cachedinverse)) {
    message("getting cached data")
    return(cachedinverse)
  }
  data <- x$get()
  cachedinverse <- solve(data, ...)
  x$setsolve(cachedinverse)
  cachedinverse
}
