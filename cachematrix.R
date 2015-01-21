## Matrix inversion is usually a costly computation. makeCacheMatrix and
## cacheSolve are functions created to  cache the inverse of the matrix
## rather than computing it repeatedly

## 'makeCacheMatrix' creates a special "matrix", which is really a list 
## containing functions to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix
## Note: creating functions within makeCacheMatrix invokes lexical scoping
makeCacheMatrix <- function(x = matrix()) {
  # Initialise the inverse 'i' to NULL
  i <- NULL
  # Create a function 'set' that sets the (new) matrix in the cache and 
  # initialises the inverse 'i' to NULL to indicate that the inverse is not 
  # yet computed
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Create a function 'get' to return the matrix in the cache
  get <- function() x
  # create a function 'setinv' to set the inverse 'i' in the cache
  setinv <- function(inv) i <<- inv
  # Create a function 'getinv' to return the inverse '1' in the cache
  getinv <- function() i
  # Create and return a list containing the above 4 functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' computes the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been computed. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it computes the inverse of the matrix
## and sets the inverse of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  # Check whether the inverse has already been computed and return that value
  # if it has been computed, otherwise proceed to compute the inverse
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  # Get the matrix and compute its inverse
  m <- x$get()
  i <- solve(m, ...)
  # Set the computed inverse 'i' in the cache and return it
  x$setinv(i)
  i
}