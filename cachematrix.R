## Programming Assignment 2

## Create a matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinv <- function(inv) {i <<- inv}
  getinv <- function() {i}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Check the cache for existing inverse matrix, if not, then call getinv()

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
