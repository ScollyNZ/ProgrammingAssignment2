## Create a cached matrix so that when cacheSolve is called the result is 
## stored and returned. The next time cacheSolve is call, for the same matrix, the result calculated 
## last time is return, instead of recalculating the inverse.
##
## Usage:
## > m <- matrix(c(4,3,3,2),nrow=2)
## > m
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > newM <- makeCacheMatrix(m)
## > cacheSolve(newM)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(newM)
## getting cached data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
##
## NOTE: the second time cacheSolve is called for newM the result is not calculated but returned from a cahced value


## Extend matrix with methods to support caching of the result of a call to solve
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                 ## Ensure we start clean
  set <- function(y) {      ## Method to store result for later retrival - e.g. newM$set(<<matrix>>)
    x <<- y
    m <<- NULL
  }
  get <- function() x       ## Method to return original matrix - e.g. newM$get()
  setsolve <- function(solve) m <<- solve   ## Method to store solve result for later - e.g. newM$setsolve(<<inverse matrix>>)
  getsolve <- function() m                  ## Method to retrieve stored result - e.g. newM$getsolve()
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Retrieve the solved matrix for a matrix created with the makeCacheMatrix method call
## If the matrix has not been solved previously, solve it, store the result and return
## If the matrix has been solved previously, just return the solved result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) { ## Has the matrix been solved before?
    message("getting cached data")  ## Yes, inform the user
    return(m)                       ## and early return with previously solved matrix
  }
  data <- x$get()                   ## else, retrieve the matrix
  m <- solve(data, ...)             ## solve it
  x$setsolve(m)                     ## store for next time
  m                                 ## and return calcuated result
}