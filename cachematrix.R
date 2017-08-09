## Script that defines a special Matrix that caches its inverse to avoid recalculation

## Special matrix initialisation: 
## Input: x : matrix to be made special
## Output: list with getters and setters for both special matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) {
    inv <<- i
  }
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function that computes the inverse matrix of x: 
## Input: x : input matrix upon which the inverse will be calculated
## Output: inverse matrix. The function also caches the inverse in the special matrix "memory"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
