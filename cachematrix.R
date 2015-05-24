## This two R functions can be used to cache potentially time-consuming 
## computations involved in getting the inverse of a matrix repeatedly 
## (e.g. in a loop). If the contents of a vector are not changing, it may 
## make sense to cache the value of the inverse so that when we need it again,
## it can be looked up in the cache rather than recomputed
##
## Example of use:
## a <- matrix(Data, nrow, ncol)      builds matrix 'a'
## m <- makeCacheMatrix(a)            builds special matrix 'm'
## m$get()                            returns 'a'
## ms$getinv()                        returns 'NULL'
## cacheSolve(m)                      calculates the inverse of 'a'
## cacheSolve(m)                      retrieves  the inverse from cache


## This first function, makeCacheMatrix, creates a special "matrix" 
## object that can be used to put the inverse of the matrix on cache

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {                # set the specified matrix values
    x <<- y
    inv <<- NULL                      # resets inverse if matrix changed
  }
  
  get <- function()                   # returns the stored matrix
    x
  
  setinv <- function(inverse)         # set inverse matrix
    inv <<- inverse
  
  getinv <- function()                # returns stored inverse matrix
    inv
  
  list(set = set, get = get,          # make functions availables for
       setinv = setinv,               # external calling
       getinv = getinv)
}


## This function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {      # Returns the inverse of matrix 'x'
  
  inverse <- x$getinv()               # if already calculated, retrieves 
  if(!is.null(inverse)) {             # the inverse of 'x' from cache
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()                     # if not available on cache,
  inverse <- solve(data, ...)         # calculates the inverse of 'x'
  x$setinv(inverse)
  inverse
  
}
