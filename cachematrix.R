## Programmer: Mark Barkell
## Date Created:  2017-03-12 (See version history in git for changes)
##
## The makeCacheMatrix and the cacheSolveMatrix are implemented to store after calculating
## a matrix inverse so that the stored matrix inverse may be used to trade RAM space for 
## lower times in repeated calculation.
##
## Please Note, the implementation of this functionality leans very much upon
## the example code which is part of the assignment.   That example code
## caches mean data.  The big difference here is mainly just variable and
## and function names.  The general grammatical pattern of the functions
## is as the course indicates in the README.md

## Returns an instance of an object set up to save or get a matrix inversion
## into memory.

makeCacheMatrix <- function(x = matrix()) {
  ci <- NULL
  set <- function(y) {
    x <<- y
    ci <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) ci <<- inv
  getinverse <- function() ci
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrive from cache, or calculate and cache, the inverse of the matrix x.
## Precondition, x must be invertable.
## parameters:  x, required: matrix to invert.
##              ... addition values to pass to the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ci <- x$getinverse()
    if (!is.null(ci)) {
      print("getting cached data")
      return (ci)
    } 
    else {
      print("no cache to serve")
    }
    d <- x$get()
    ci <- solve(d, ...)
    x$setinverse(ci)
    ci
}
