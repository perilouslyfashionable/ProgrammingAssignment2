## Functions which implement the requirements of Programming 
## assignment 2 from coursera R programming: a function which
## returns a list of functions for manipulating stored matrix
## data and a function which uses the list to cache the results
## of a matrix inverse operation.


## makeCacheMatrix: returns a list of functions providing
 # facilities to set and get a matrix value and get and 
 # set its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set: set x in the enclosing environment to 
  #      the argument, y, and set the calculated
  #      inverse matrix, m, to NULL (as it will)
  #      no longer be valid
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get: return x from the enclosing, the matrix
  #      environment, as set by set()
  get <- function() {
    x
  }
  # setInverse: set m to the matrix inverse argument
  setInverse <- function(matrix.inverse) {
    m <<- matrix.inverse
  }
  # getInverse: return m, the inverse of the matrix
  getInverse <- function () { 
    m
  }
  
  # return a list of the functions created above
  list (set = set, 
        get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}


## Takes the list returned from the makeCacheMatrix function
 # and returns the inverse of the matrix contained in it, 
 # returning the cached value where available.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    return (m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setInverse(m)
  m
}
