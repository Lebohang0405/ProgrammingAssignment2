## Put comments here that give an overall description of what your
## functions do

## The computation of the inverse matrix using library(Mass) is detailed below
## There are two functions in the below code. makeCacheMatrix and makeCacheInverse
## makeCacheMatrix consists of set, get, setInverse, getInverse
## Assume Matrix supplied is invertable

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initializing inverse as NULL
  set <- function(y){
    x <<- y 
    inv <<- NULL #'superassignment' operator
  }
  
  get <- function() {x}   #Gets the matrix x (function)
  setInverse <- function(inverse) {inv <<- inverse} 
  getInverse <- function() {inv}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
      message("Getting cached data")
      return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)   #calculates inverse value
  x$setInverse(inv)
  inv
  
}
