## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL        ## initializing inverse as NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x    ## function to get matrix X
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  tat <- x$get()
  z <- solve(tat,...)
  x$setInverse(z)
  z
}
