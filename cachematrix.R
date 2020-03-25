
#The following code caches the inverse of a matrix (We suppose that the matrix is invertible)

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #  Set the value and clear from the cash
  set <- function(y) {
    x <<- y   
    m <<- NULL 
  }
  # Get the value of the matrix
  get <- function() x
  
  # Set the inverse.

  setInverse <- function(inverse) m <<- inverse
  # Get the inverse
  getInverse <- function() m
  
  # Return a list with the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x) {
  m <- x$getInverse() # Cached value for the inverse
  if(!is.null(m)) { # Return the cached inverse if not empty
    message("getting cached data")
    return(m)
  }
  # If the cached value is empty, then we need to calculate it
  data <- x$get()  
  m <- solve(data) 
  x$setInverse(m)  
  m              
}