## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#x: a square invertible matrix return: a list containing functions to
##set the matrix
##get the matrix
##set the inverse
##get the inverse
##this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## x output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  # if the inv has been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # else, calculate the inv 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # set the value of the inv in the cache through the setinv function.
  x$setinv(inv)
  
  return(inv)
}
