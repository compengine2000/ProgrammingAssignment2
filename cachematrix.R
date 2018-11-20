## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    print("Cached Data Found")
    # print(m)
    return(m)
  }
  print("No cached matrix found")
  mat <- x$getMatrix()

  x$setInverse(solve(mat))
  x$getInverse()
}
