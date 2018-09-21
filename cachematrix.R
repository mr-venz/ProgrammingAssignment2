## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Similarly to the example provided, this function will return a list of for objects.
##Each one of these objects is a function itself. They shall be called using the command X$object_name.
##set will allow the user to set the matrix to be processed and stored in cache. get will return such matrix.
##setInverse will compute and store the inverse of a matrix and cache it.
##getInverse will return the inverse matrix previously computed.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(x) inverse <<- solve(x)
  getInverse <- function (x) inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
##The function will use x$getInverse to get the cached inverse value. If this does not exist (i.e. it is NULL),
##This will be computed and cached using x$setInverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cache data")
    inv
  }
  inv <- solve(x$get())
  x$setInverse(inv)
  inv
}
