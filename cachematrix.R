## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" which is actually a list
# containing matrix itself, getters and setters for this matrix
# and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  getinverse <- function() i
  
  setinverse <- function(inv) i <<- inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function
# cacheSolve - finds inverse for given cachematrix
# first tries to get cached value and if it is null calls solve 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if (is.null(x_inv)) {
    x_inv <- (solve(x$get(),...))
    x$setinverse(x_inv)
    return(x_inv)
  }
  else {
    x_inv
  }
}

