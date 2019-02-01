## Together, these two functions cache the inverse of a matrix so that it can be
## retrieved when needed, instead of having to be recalculated.

## makeCacheMatrix initialises a matrix, x and an anonymous variable, i, and
## defines 'getters' and 'setters' to retrieve and reset cached values of x
## and what will become its inverse, i. User should store output of this
## function as a new object to be used in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve takes the output of makeCacheMatrix and, if already calculated
## through a previous call to cacheSolve, retrieves the inverse, i, of matrix
## x. If not previously calculated, the inverse, i, is calculated and returned.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
