## These two functions will check and find a inverse of a matrix if the matrix has one (inverse)

## The following function will take a matrix as a input and turn it into a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) {
  
  Inv <- NULL
  set <- function(y) {
    
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) Inv <<- solve
  getsolve <- function() Inv
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)

}


## The folling function takes the list from the previous matrix and by the previous function as 
## the input, then check to see if its inverse is already in Cache, if yes, retrive the inverse
## if not, calculate its inverse then put the result into cache.

cacheSolve <- function(x, ...) {
  
  Inv <- x$getsolve()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setsolve(Inv)
  Inv
  return(Inv) ## Return a matrix that is the inverse of 'x'
}

