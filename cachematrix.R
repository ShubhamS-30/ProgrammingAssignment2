## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
'''
The line - dim(y)<-c(sqrt(length(y)),sqrt(length(y)))
changes the dimension of the input vector to that of a square matrix.

solve() returns the inverse of the input matrix.
'''

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    dim(y)<-c(sqrt(length(y)),sqrt(length(y)))
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
'''
cache solve function first checks if inverse is already calaculated
or not.

If calculated calls variable " m " from cache to get the value.

If not calculate then it calls the setinv function inside the makeCacheMatrix
function to calcute the inverse. Calculated inverse is assigned to "m" which is 
then returned by the cacheSolve function.
'''


cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

