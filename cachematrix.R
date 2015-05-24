## These functions below creates a special "matrix", chaches its inverse and computes 
## its the inverse.

## This function creates a special "matrix" object that can cache its inverse.
## Setmean and getmean don't calculate the mean. They just store the value in a 
## variable m (setmean) and return it (getmean). Solve and getsolve can store
## the value of the inverse of the matrix in a variable s (solve) and return it
## (getsolve).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  s <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    s <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  solve <- function (solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean,
       solve = solve,
       getsolve = getsolve)
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$solve(s)
  s
}
