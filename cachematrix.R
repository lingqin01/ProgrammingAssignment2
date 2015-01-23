## This function creates a special matrix object that can cache 
## its inverse, it also includes methods to access the matrix and
## its inverse, set a matrix and its inverse
## @author Ling Zhang
## @date 01/22/2015

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function(y) {
     z <- x-y
     if(sum(z)==0 & !is.null(inverse) ) # matched
       return(inverse)
     else return(NULL)
  }
  list(set = setMatrix, get = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve  
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- x$getInverse(data)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
##aMatrix <- makeCacheMatrix()
##m <- matrix(1:4,2,2)
##aMatrix$set(m)
##aMatrix$get()
##cacheSolve(aMatrix)
##cacheSolve(aMatrix)
