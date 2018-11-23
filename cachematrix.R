## This is programming assignment 2.  Storing scoped variables and returning them.

## makeCacheMatrix can store a matrix and its inverse
## setMatrix stores a new matrix and clears the previous inverse if one was already stored
## getMatrix returns the stored matrix
## setInverse stores the inverse of the stored matrix
## getInverse returns the inverse matrix
makeCacheMatrix <- function(original = matrix()) {
  inverse <- NULL
  setMatrix <- function(updatedVal) {
    original <<- updatedVal
    inverse <<- NULL
  }
  getMatrix <- function() original
  setInverse <- function(newInverse) { inverse <<- newInverse  }
  getInverse <- function() inverse

  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the previously calculated inverse if one exists, otherwise it performs the calculation and stores it.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    print("Cached Data Found")
    return(m)
  }
  print("No cached matrix found")
  mat <- x$getMatrix()

  x$setInverse(solve(mat))
  x$getInverse()
}
