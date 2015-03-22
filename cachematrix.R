## Below are two functions that cache, and calculate the inverse of a matrix.

## function to create a "matrix" that caches its inverse.
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(x) {
    m <<- x;
    inverse <<- NULL;
  }
  get <- function() return(m);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(m, ...) {
  inverse <- m$getinv()
  if(!is.null(inverse)) {
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinv(inverse)
  return(inverse)
}
