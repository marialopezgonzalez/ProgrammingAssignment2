## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minver <- NULL
  setM <- function(y) {
    x <<- y
    minver <<- NULL
  }
  getM <- function() x
  setminver <- function(minverse) minver <<- minverse
  getminver <- function() minver
  list(setM = setM, getM = getM,
       setminver = setminver,
       getminver = getminver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minver <- x$getminver()
  if(!is.null(minver)) {
    message("getting cached data")
    return(minver)
  }
  matriz <- x$getM()
  minver <- solve(matriz,...)
  x$setminver(minver)
  minver
}
