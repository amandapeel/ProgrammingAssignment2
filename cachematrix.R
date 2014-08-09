## These are a set of functions designed to first transform a matrix
## such that it can cache its inverse, and second to cache the inverse

## makeCacheMatrix creates a matrix object that can cache the matrix's (x) inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(mean) m <<- mean
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve computes and caches inverse of matrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##The following will run the program on a sample set
##> a <- c(1,2,0)
##> b <- c(0,1,9)
##> c <- c(5,0,4)
##> mtx <- matrix(c(a,b,c), nrow=3, ncol=3)
##> mtx2 <- makeCacheMatrix(mtx)
##> cacheSolve(mtx2)
