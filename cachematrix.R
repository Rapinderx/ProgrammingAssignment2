## the makeCacheMatrix creates a special matrix object that can caches its inverse
## the CacheSolve function computes the inverse of the special matrix returned 
              ## by makeCacheMatrix above

## the makeCacheMatrix sets and gets the value of the matrix and inverse 

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve computes the inverse of x, if inverse had already been calculated then 
        ##it will retrieve the inverse from cache 

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

