## I copied the two provided functions from Coursera and changed them a bit so they:
## A: Can take a matrix instead of a vector
## B: Calculates the inverse instead of the mean

## Almost nothing has changed about this function. It now takes a matrix as an argument
## and calls the solve() function on m


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve #Right here! This used to be mean
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Again, does almost the same exact thing, except it returns the transposed version of the matrix
## by calling the t() function on m. For some reason, the return values are added in reverse, t() takes care of that.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  t(m)
}

myMatrix <- matrix(c(4,10,4,5), 2, 2)
cacheSolve(makeCacheMatrix(myMatrix))

