## The following two functions cache the inverse of a matrix.

## This first function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  make <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  obt <- function() x
  makeInverse <- function(inverse) Inverse <<- inverse
  obtInverse <- function() Inverse
  list(make = make,
       obt = obt,
       makeInverse = makeInverse,
       obtInverse = obtInverse)
}

## This second computes the inverse of the matrix 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  Inverse <- x$obtInverse()
  if (!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  glb <- x$obt()
  Inverse <- solve(glb, ...)
  x$makeInverse(Inverse)
  Inverse
}

