makeCacheMatrix <- function(x = matrix()) {
  #x = a square invertible matrix
  #return = a list used as an input to cacheSolve() containing:
  #1) set matrix
  #2) get matrix
  #3) set inverse
  #4) get inverse
  inv <- NULL
  set <- function(y) {
    #use '<<-' to assign a value to an object in an environment
    #different from the current environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cachesolve <- function(x, ...) {
  #x=output of makeCacheMatrix()
  #returns=inverse of the matrix input to makeCacheMatrix()
  inv <- x$getinv()
  #if the inverse was already calculated
  if(!is.null(inv)) {
  #get it from cache and skip caclulation
    message("getting cached data")
    return(inv)
  }
  #otherwise calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  #sets the value of the inverse according to cache
  x$setinv(inv)
  inv
  #returns a matrix that is the inverse of 'x'
}
