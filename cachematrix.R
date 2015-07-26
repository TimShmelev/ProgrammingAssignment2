# Programming Assignment #2

## Functions 'makeCacheMatrix' and 'cacheSolve' optimize calculation of inverse matrices:
## Calculation is needed one time for each matrix. Subsequent queries are returned from cache.

## Function 'makeCacheMatrix' creates a cache for inverse matrices
makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL
      
      setMatrix <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
      }
      
      ## Names of the following functions are self-explanatory
      getMatrix <- function() x
      setInverseMatrix <- function(im) inverseMatrix <<- im
      setInverseMatrix <- function() inverseMatrix
      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}

## Function 'cacheSolve' calculate inverse matrix for the 1st time and returns
## cached matrices for subsequent queries. 
cacheSolve <- function(x, ...) {
      inverseMatrix <- x$getInverseMatrix()
      if(!is.null(inverseMatrix)) { 
            message("getting cached data")
            return(inverseMatrix)
      }
      
      ## Initial calculation of inverse matrix and caching the result:
      data <- x$getMatrix()
      inverseMatrix <- solve(data)
      x$setInverseMatrix(inverseMatrix)
      
      inverseMatrix
}
