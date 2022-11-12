## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse # set the value of the inverse
  getinverse <- function() inv # get the value of the inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # function() inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # return value
  }
  data <- x$get() # function() x
  inv <- solve(data, ...) # calculates/stores inverse
  x$setinverse(inv) # function(inverse)
  inv
}

