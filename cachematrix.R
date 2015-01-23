## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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
     if(sum(z)==0) # matched
       return(inverse)
     else return(NULL)
  }
  
  list(set = setMatrix, get = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse(x)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
