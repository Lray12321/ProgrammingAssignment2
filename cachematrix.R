## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Create inverse property
  Inv <- NULL
  
  ## Set matrix
  set <- function( y ) {
    x <<- y
    Inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {x}
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {Inv <<- inverse}
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {Inv}
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  
  ## ]Return the inverse if its already set
  if(!is.null(Inv) ) {
    message("getting cached data")
    return(Inv)
  }
  
  ## Get the matrix from our object
  mat <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  Inv <- solve(mat,...)
  
  ## Set the inverse to the object
  x$setInverse(Inv)
  
  ## Return the matrix
  Inv
}
