## cachematrix: caches the inverse of a matrix!!!

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y=matrix()) {
    x <<- y 
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invmatrix <<- inv
  getinverse <- function() invmatrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCac-
## -heMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the 
## cache

cacheSolve <- function(x, ...) {
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data.")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
}
