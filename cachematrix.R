## This file contains functions that can be used to compute
## and cache the inverse of a matrix; 
## which is a potentially time-consuming operation.
## The functions assume that the input matrix is invertible.

## makeCacheMatrix creates a special matrix object that can
## cache it's own inverse. It creates a list of functions.
## It contains the following 4 functions:
## 1. set - Changes the matrix in the main function.
##          It takes a matrix as the input.
## 2. get - Returns the matrix stored in the main function.
## 3. setinverse - Changes the inverse of the matrix in the main function.
##                 It takes the new inverse matrix as the imput.
## 4. getinverse - Returns the value of the inverse stored in the main function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve takes a matrix as input matrix and returns its inverse.
## If the inverse is already cached, cacheSolve returns the cached inverse
## along with a message "getting cached inverse".
## If the inverse is not in the cache, cacheSolve computes the inverse and saves a copy.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
