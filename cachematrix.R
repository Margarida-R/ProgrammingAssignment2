## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the cached inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse if matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return list of all the above functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # If inverse is already cached
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # If inverse is not cached, compute it
  mat <- x$get()
  inv <- solve(mat, ...)  # Calculate inverse
  x$setinverse(inv)       # Cache the inverse
  inv
}