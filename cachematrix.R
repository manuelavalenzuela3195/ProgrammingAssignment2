## The following code creates makeCacheMatrix to inverse a matrix
## instead of compute it repeatedly

## matrix object creation in order to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
}
 get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## CacheSolve function gets the inverse of the matrix created before: makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}
