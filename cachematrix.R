## The function creates a "matrix" object that can cache its inverse.This function only

## calculate the inverse once to get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  CacheInverse <- NULL
  set <- function(y){
    x <<- y
    CacheInverse <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) CacheInverse <<- inverse
  getInverse <- function() CacheInverse
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)){
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data,...)
  x$setInverse(invFunc)
  invFunc
}

## An example:
M <- matrix(c(4,7,2,6),nrow=2,ncol=2)
CacheMatrix <- makeCacheMatrix(M)
cacheSolve(CacheMatrix)

