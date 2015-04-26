##              1. set the value of matrix
##              2. get the value of matrix
##              3. set the value of inverse
##              4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  invmatrix = NULL
  set = function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get = function() x
  setinv = function(inverse) invmatrix <<- inverse 
  getinv = function() invmatrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Function checks if Inverse of a matrix is already cached
## Calls the first function to find a cached value
## If not, then it computes

cacheSolve <- function(x, ...) {
        
  invmatrix = x$getinv()
  
  if (!is.null(invmatrix)){
    return(invmatrix)
  }
  
  
  matrix.data = x$getinv()
  invmatrix = solve(matrix.data, ...)
  
 
  x$setinv(invmatrix)
  
  return(invmatrix)
}
