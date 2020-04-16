## Write a pair of functions that cache the inverse of a matrix


## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix to null
  m <- NULL
  
  ## Set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## Get the matrix
  get <- function()x
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  
  ## Return a list of the methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() 
  
  ## If its already set, return the inverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  mat <- x$get()
  
  ## Calculate the inverse matrix
  m <- solve(mat,...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
