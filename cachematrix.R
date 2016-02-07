## Functions makeCacheMatrix() and cacheSolve() create a matrix object
##   and find its inverse if needed, repsectively

## Function creates a list of functions which operate on
##   the function environment's matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y){
    x <<- y
    xInv <<- NULL  ## Done because the matrix changed
  }
  get <- function() x
  setInv <- function(newInv) {
    xInv <<- newInv
  }
  getInv <- function() xInv
  
  # Return values as list:
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Function finds the matrix inverse of an object created
##   by makeCacheMatrix() above, only if the inverse has
##   not already been calculated
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)){
    #message("getting the cached inverse")
    return(x$getInv())
  }
  # else, we calculate its inverse and set:
  inv <- solve(x$get(),...)
  x$setInv(inv)
  return(inv)
}
