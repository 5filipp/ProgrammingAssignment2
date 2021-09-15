## makeCacheMatrix and cacheSolve functions can cache potentially time-consuming computations
##and return it instead of repeating same computational step


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("get cached value")
    return(inv)
  }
  mat = x$get()  
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  }

pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
pmatrix$getInverse()
cacheSolve(pmatrix)
