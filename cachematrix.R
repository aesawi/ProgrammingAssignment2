## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat_inv <<- inverse
  getinverse <- function() mat_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mat_inv <- x$getinverse()
  if(!is.null(mat_inv)) {
    message("getting cached data.")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data)
  x$setinverse(mat_inv)
  mat_inv
}
