###############################################################################
## File Description
## 
## This fie contains two functions that are used to create a special object 
## (list) that stores a matrix and cache's its inverse.
##
###############################################################################

###############################################################################
## Function Comments
##
## makeCacheMatrix creates a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix
##
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

###############################################################################
## Function Comments
##
## cacheSolve calculates the inverse of the special "matrix" created with the 
## makeCacheMatrix function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinv function.
###############################################################################
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
