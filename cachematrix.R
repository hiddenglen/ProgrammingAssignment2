# The following set of functions attempt to provide a
# solution for caching the results of matrix inverse operation
# in order to avoid recomputation when underlying matrix has not changed. 
# ( i.e until set method is not called)
# 


## Returns a list containg four functions as follows. 
## 1. set() to set the matrix to be operated on
## -- assumed to be a matrix whose inverse can be obtaned by solve()
## 2. get() to return the matrix provided to the set() function
## setinverse(inv) - called with the inverse matrix, i.e to be cached..
## getinverse() returned the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the list created by makeCacheMatrix
## and 
## 1. returns inverse matrix when it is available, via the getinverse() function
## or otherwise
## 2. computes by calling solve(), 
##    --sets via the setinverse() function returns the inverse matrix.

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mymatrix<-x$get()
  m<-solve(mymatrix, ...)
  x$setinverse(m)
  m
}