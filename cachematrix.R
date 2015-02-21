## Matrix inversion is usually a costly computation 
## The following functions can cache the inverse matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cacheMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  getMatrix <- function() x
  
  setCacheMatrix <- function(inversedMatrix) cacheMatrix <<- inversedMatrix
  getCacheMatrix <- function() cacheMatrix
    
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheMatrix <- x$getCacheMatrix()
  if(!is.null(cacheMatrix)) {
    message("getting cached data")
    return(cacheMatrix)    
  }
  myMatrix <- x$getMatrix()
  cacheMatrix <- solve(myMatrix)
  x$setCacheMatrix(cacheMatrix)
  cacheMatrix
}
