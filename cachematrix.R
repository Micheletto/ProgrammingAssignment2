## matrix and caching inverse matrix functions.

## makeCacheMatrix creates a list containing functions that
## set and return a matrix or the inverse of that matrix
## set - set the matrix value.
## get - return the matrix value.
## getinv - return the inverse matrix, must first be 
##          computed with the cacheSolve() function.
## setinv - set the inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
  myinvmat <- NULL
  set <- function(y) {
    x <<- y
    myinvmat <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(i) myinvmat <<- i
  getinv <- function() myinvmat
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Calculates, or retrieves from cache, the inverse of a matrix
## created by the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(is.null(i)){
    i <- solve(x$get())
    x$setinv(i)
  } else {
    message("Getting cached inverse matrix.")
    return(i)
  }
}