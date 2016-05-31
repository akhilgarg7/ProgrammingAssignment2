## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Description:
## Input Variables: x of type matrix.
## Output: A list of functions.
## Return a list(setMatrix=value, getMatrix=value, setInverse=value, getInverse=value)
## This function has two variables(inverse and x) within its scope.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #inverse: each variable of makeCacheMatrix will have it's own copy
  #e.g "xyz <- makeCacheMatrix()" xzy will have a variable "inverse" within it's scope
  
  setMatrix <- function(y) {
    #"y" is within the scope of function setMatrix()
    #"setting x by using "<<-" so that it set the makeCacheMatrix's "x" as "inverse"
    #if x were set using "<-", a local copy of x would have been created within the scope of setMatrix()'s function
    x <<- y
    inverse <<- NULL
  }
  
  
  # returns global x
  getMatrix <- function() x
  
  
  # set global "inverse"
  setInverse <- function(inversedValue) inverse <<- inversedValue
  
  # returns global "inverse"
  getInverse <- function() inverse
  
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}




## Write a short comment describing this function
## Input: x is a matrix created by function makeCacheMatrix()
## Return: a matrix that is the inverse of 'x'
## This function get the value from getInverse() function
## if the value is not null then returns it otherwise it calculate the inverse using standard api
## set the value of inverse in x's variable "inverse" and then return inversed value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversed <- x$getInverse()
  if( !is.null(inversed) ) {
    message("Getting Cached Data")
    return (inversed)
  }
  
  matrixInput <- x$getMatrix()
  inversed <- solve(x$getMatrix(), ...)
  x$setInverse(inversed)
  inversed
}





## Running example
#k <- makeCacheMatrix(matrix(1:4, 2,2))
#z1 <- cacheSolve(k) # it will actually calculate the inverse
#z2 <- cacheSolve(k) # it will return the inverse from cache.


