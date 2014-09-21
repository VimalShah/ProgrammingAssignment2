##  makeVector creates a special "Matrix" returns a list containing a function to
##
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinverse <- function(inverse) i <<- inverse
  
  # Get the inverse
  getinverse <- function() i
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve funtcion computes the inverse of the matrix.
## If the inverse is already calculated before, it returns the cached value.

cacheSolve <- function(x, ...) {
  
  # getinverse value from global environment
  i <- x$getinverse()
  
  # return value if the inverse value is already calculated
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # get the matrix to calculate Inverse
  data <- x$get()
  
  # calculate Inverse
  i <- solve(data, ...)
  
  # cache the inverse in global environment 
  x$setinverse(i)
  
  # return inverse        
  i
}
