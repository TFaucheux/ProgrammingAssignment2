## Below are 2 functions that cache, and calculate the inverse of a matrix.

## Testing
##   1. Create a 3x3 matrix
##   mat<-matrix(c(1,4,9,0,-3,2,2,7,8),3,3)
##   2. Display the output of the matrix
##   mat
##   3. Create a Cached version of the Matrix using Function #1  
##   matInv<-makeCacheMatrix(mat)
##   4. Use function #2 to return inverse matrix
##   cacheSolve(matInv)
##   5. Run a second time to see the "using cached data" message
##   cacheSolve(matInv)

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
    message("Using cached data...")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinv(inverse)
  return(inverse)
}
