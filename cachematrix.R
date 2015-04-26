## Functions that allow for caching of matrix inversion to speed up calculations

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(storedMatrix = matrix()) {
  storedInverse <- NULL
  get <- function() {
    storedMatrix
  }
  set <- function(newMatrix) {
    # assume that newMatrix is not the same as storedMatrix
    # also, do not calculate the inverse immediately, it will be calulcated, if needed, by cacheSolve()
    storedMatrix  <<- newMatrix
    storedInverse <<- NULL
  }
  getInverse <- function() {
    storedInverse
  }
  setInverse <- function(inverse) {
    storedInverse <<- inverse
  }
  list(get        = get,
       set        = set,
       getInverse = getInverse,
       setInverse = setInverse
  )
}


## An alternative to solve() that uses the special caching "matrix" created with makeCacheMatrix()

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  originalMatrix <- matrix$get()
  inverse <- solve(originalMatrix, ...)
  matrix$setInverse(inverse)
  inverse
}
