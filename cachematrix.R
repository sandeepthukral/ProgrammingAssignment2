## The function makeCacheMatrix creates a special Matrix that really has
## a function to 
### set the value of the matrix
### get the value of the matrix
### set the value of the inverse
### get the value of the inverse
##
## The function calculates the inverse of the special "matrix" created with the 
## function makecacheMatrix, but only when the same has not been cached earlier.


## The following function makeCacheMatrix creates a special Matrix that really has
## a function to 
### set the value of the matrix
### get the value of the matrix
### set the value of the inverse
### get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInv) inverse <<- newInv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the 
## function makecacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  ## return cached data if it exists
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## compute the inverse and store it in the Matrix
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  
  ## return the inverse
  inverse
}
