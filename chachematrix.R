## Assignment week 2



## Use: 
## - makeCacheMatrix: This function creates a special "matrix" object that can cache its 
##   inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  my_set <- function(y) {
    x <<- y
    i <<- NULL
  }
  my_get <- function() x
  my_setinverse <- function(inverse) i <<- inverse
  my_getinverse <- function() i
  list(set = my_set,
       get = my_get,
       setinverse = my_setinverse,
       getinverse = my_getinverse)
}



## - cacheSolve: This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##   has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  my_data <- x$get()
  i <- solve(my_data, ...)
  x$setinverse(i)
  i
}



# Example of use
data_test <- matrix(c(1,2,3,4),2,2)

data_test_1 <- makeCacheMatrix(data_test)
#Inverse from the first matrix
cacheSolve(data_test_1)
#Inverse from the cached matrix
cacheSolve(data_test_1) #inverse returned from cache