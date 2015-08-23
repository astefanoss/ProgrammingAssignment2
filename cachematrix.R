## These functions calculate and cache the inverse matrix of a square (2 rows, 2 columns)
## matrix. This is achieved through the scoping rules of R (lexical scoping).
## makeCacheMatrix creates a special matrix that allows caching the inverse matrix of the input matrix 'x'.
## cacheSolve calculates the inverse matrix of the input matrix 'x'. If the inverse of 'x'
## has already been determined previously, then cacheSolve will get the inverse matrix from cache,
## avoiding the need to recompute the inverse.



## makeCacheMatrix is actually a list to set and get a matrix as well as setting the inverse matrix and getting
## it from cache. The user must assign the makeCacheMatrix function with the input square matrix (argument)
## to a variable in the R console (working space). For example:
##
## matrixExample <- makeCacheMatrix(matrix(1:4,2,2))
## 
## If the user wants to change the elements of the matrix, 
## then typing matrixExample$setMatrix(y), where y is a new
## square matrix, in the console will set the new matrix in matrixExample.
## Typing matrixExample$getMatrix(), will return the setted matrix.
## If the inverse matrix has been previously calculated with the cacheSolve function, 
## then typing matrixExample$getInverse() will return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse = matrix()){
    inv <<- inverse
  }
  getInverse <- function() inv
  list (setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x' using the solve() function.
## If the inverse of 'x' has been previously computed,
## then cacheSolve will retrieve the cached inverse matrix.
## Using the example from above, type in the R console: cacheSolve(matrixExample)
## If this is the first time the matrix in matrixExample is used, then cacheSolve will calculate the inverse
## and store it in cache using setInverse function from makeCacheMatrix.
## However, if the inverse of the matrix in matrixExample has been previously calculated, then the inverse
## will be retrieved from cache using the function getInverse from makeCacheMatrix.
## If the matrix in matrixExample has been changed by the user, then the inverse will be
## calculated for the new matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Retrieving cached inverse matrix")
    return(inv)
  }
  yourMatrix <- x$getMatrix()
  inv <- solve(yourMatrix)
  x$setInverse(inv)
  inv
}
