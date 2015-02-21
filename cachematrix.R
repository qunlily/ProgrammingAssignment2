## Matrix inversion is usually a costly computation. The following functions 
## can cache an inversed matrix object in memory for repeat fetches
## Here is an example:
# > c=rbind(c(1,2,3),c(1,3,2),c(3,2,1))
# > c
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    1    3    2
# [3,]    3    2    1
# > m<-makeCacheMatrix(c)
# > cacheSolve(m)
# [,1]       [,2]        [,3]
# [1,]  0.08333333 -0.3333333  0.41666667
# [2,] -0.41666667  0.6666667 -0.08333333
# [3,]  0.58333333 -0.3333333 -0.08333333
# > cacheSolve(m)
# getting cached data
# [,1]       [,2]        [,3]
# [1,]  0.08333333 -0.3333333  0.41666667
# [2,] -0.41666667  0.6666667 -0.08333333
# [3,]  0.58333333 -0.3333333 -0.08333333

##
## makeCacheMatrix() 
##  initiates a list of functions, with which
##  you can set/get a matrix object and cache the matrix object.
##
makeCacheMatrix <- function(x = matrix()) {
  
  ## cache object holder
  cacheMatrix <- NULL
  
  ## set new matrix object and clean cached matrix
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  ## get new matrix object
  getMatrix <- function() x  
  ## cache computed matrix object
  setCacheMatrix <- function(newMatrix) cacheMatrix <<- newMatrix
  ## get cached matrix object
  getCacheMatrix <- function() cacheMatrix
    
  ## return list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix)
}

##
##  cacheSolve() 
##    computes the inverse of the "matrix" returned by makeCacheMatrix() above.
##    If the inverse has already been calculated for the same matrix,
##    then the cached matrix returns without compute inverse again.
##  
cacheSolve <- function(x, ...) {
  
  ## look into cache first
  cacheMatrix <- x$getCacheMatrix()
  if(!is.null(cacheMatrix)) {
    message("getting cached data")
    return(cacheMatrix)    
  }
  
  ## if not cached, compute the inverse of the matrix
  myMatrix <- x$getMatrix()
  inverseMatrix <- solve(myMatrix)
  
  ## cache the inversed matrix object
  x$setCacheMatrix(inverseMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  return(inverseMatrix)
}
