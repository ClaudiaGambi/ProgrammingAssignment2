# The first function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

# The second function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # inv will hold the cached inverse of the matrix
  inv <- NULL
  
  # set is a function that assigns a new matrix y to x and resets inv to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return matrix x
  get <- function() x
  
  # assign the inverse matrix to inv
  setinverse <- function(inverse) inv <<- inverse
  
  # return the cached inversed matrix
  getinverse <- function() inv
  
  # return a list with set, get, setinverse and getinverse 
  # so the matrix and its inverse can be manipulated
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# retrieves the inverse of the special "matrix" returned by makeCacheMatrix above. Or computes the inverse
cacheSolve <- function(x, ...) {
  
  # check if the inversed has been cached already
  inv <- x$getinverse()
  
  # if it has been cached, it will retreive the inversed matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not, this line retrieves the original matrix by calling the get function
  data <- x$get()
  
  # computes the inverse
  inv <- solve(data)
  
  # cache the computed inverse
  x$setinverse(inv)
  
  # computed inverse is returned
  inv
}
