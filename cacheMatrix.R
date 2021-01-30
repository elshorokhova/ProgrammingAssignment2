## make matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  getm <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, getm = getm,
       setinv = setinv,
       getinv = getinv)
}

##  inverse of a matrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getm()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

