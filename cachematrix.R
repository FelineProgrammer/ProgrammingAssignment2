## Put comments here that give an overall description of what your
## functions do

##This function x is the matrix and the solved t is a null. 

makeCacheMatrix <- function(x = matrix(1:9, 3, 3)) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) t <<- inverse
  getinverse <- function() t
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  t <- x$getInverse()
  if(!is.null(t)){
    message("getting cached data")
    return(t)
  }
  mat <- x$get()
  t <- solve(mat,...)
  x$setInverse(t)
  t
}
