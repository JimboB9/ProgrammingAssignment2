## makeCacheMatrix and cacheSolve are functions that make sure that the inverse
## of a (squared and invertible) matrix is only computed once to avoid multiple
## computations of the same matrix, which is computationally expensive.

## makeCacheMatrix takes a (squared and invertible) matrix as input and returns 
## a list of functions.

makeCacheMatrix <- function(x = matrix()) {
   I <- NULL
    set <- function(y) {
      x <<- y
      I <<- NULL
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a makeCacheMatrix object as input and checks, if the inverse 
## has already been computed. If yes, it displays "getting cashed data" and return
## the inverse from the cache. If no, the inverse is computed, cached and displayed.

cacheSolve <- function(x, ...) {
  
    I <- x$getinv()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I  
}


## Test functions

#l <- matrix(1:9, 3, 3, byrow=T)
#diag(l) <- 0  # make sure matrix is invertible
#lcache <- makeCacheMatrix(l)
#cacheSolve(lcache)





