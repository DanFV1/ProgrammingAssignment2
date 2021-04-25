## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

## Special matrix object -> Cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## Initialize
    i <- NULL
    
  ## Set the matrix
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
  ## Get the matrix
    get <- function() x
    
  ## Set the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    
  ## Get the inverse of the matrix
    getInverse <- function() i
    
  ## List methods
    list( set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get Inverse
  i <- x$getInverse()
  if(!is.null(i)) {
    message ("getting cached data")
    return(i)
  }
  
  ## Get the matrix
  matrix <- x$get()
  
  ## Calculate the inverse
  i <- solve(matrix, ...)
  
  ## Set the inverse
  x$setInverse(i)
  
  ## Matrix
  i
}
