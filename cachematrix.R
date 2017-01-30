## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## A function to create a matrix with capability to set and get its 
## cached inverse matrix. A set function is created for clearing the
## cache when reseting the value.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(value) {
    x <<- value
    iv <<- NULL
  }
  setInverse <- function(inv) iv <<- inv
  getInverse <- function() iv
  list(value = x, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Return the inverse matrix of x. If no value was already cached, 
## then calculate the inverse matrix and store the value in x. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getInverse()
  
  if(is.null(iv)) {
    message("calculating inverse matrix")
    iv <- solve(x$value, ...)
    x$setInverse(iv)
    return(iv)
  }
  
  message("cached inverse matrix")
  iv
}
