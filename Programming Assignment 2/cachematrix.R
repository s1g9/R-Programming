## This R file can cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  matrix(c(set, get,setinverse,getinverse),2,2)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
       m <- x[2,2]
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x[2,1]
  m <- solve(data, ...)
  x[1,1] <- m
  m
}