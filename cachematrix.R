## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # this will store the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL   # reset cached inverse whenever matrix changes
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If we already have the inverse cached, return it
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Otherwise compute inverse, cache it, and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv        ## Return a matrix that is the inverse of 'x'
}
