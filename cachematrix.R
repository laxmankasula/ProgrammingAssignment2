## Caching Inverse of a Matrix
# Test the program using following example
# originalMatrix <- matrix(c(1, 2, 3, 5), nrow = 2, ncol = 2, byrow = TRUE)
#originalMatrix
#        [,1] [,2]
# [1,]    1    2
# [2,]    3    5
# cachedMatrix <- makeCacheMatrix(originalMatrix)
# cacheSolve(cachedMatrix)
#        [,1] [,2]
# [1,]   -5    2
# [2,]    3   -1
# cacheSolve(cachedMatrix)
# inverse is cached
#       [,1] [,2]
# [1,]   -5    2
# [2,]    3   -1


# Creates a Matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## A function to compute the Inverser of a Matrix. If Inverse is calculated already, the cached inverse is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}
