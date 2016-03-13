## These functions are used in conjugation to cacche the inverse of a matrix
## and retrieve it (instead of calculating it) provided the matrix has not changed.

## "makeCacheMatrix" takes a matrix as its argument. The matrix to be passed must be
##  - a square matrix
##  - an invertible matrix
## This function returns a list containing four functions which respectively
##  - 1. set the value of the matrix
##  - 2. get the value of the matrix
##  - 3. set the value of the inverse
##  - 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) inver <<- inv
  
  getinv <- function() inver
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## "cacheSolve" is used to obtain the inverse of a matrix. It calculates the inverse
## using the "solve" function and caches the inverse. Upon all subsequent calls to
## calculate the inverse of the same matrix, it simply fetches the inverse stored in cache.

## "cacheSolve" takes the output of makeCacheMatrix as its argument. And, it returns
## the inverse

cacheSolve <- function(x, ...) {
  inver <- x$getinv()
  if(!is.null(inver)) {
    message("getting cached inverse")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinv(inver)
  inver
}
