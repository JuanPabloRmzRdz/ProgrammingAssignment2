## Together these functions will calculate the inverse of a matrix in a faster 
## way in case the data is big

## This function, sets the value of the matrix, gets its value, sets the value of
## the inverse of the matrix and gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Calculates the inverse of the matrix, if the matrix was calculated before, it
## takes it from the cache and prints it

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
