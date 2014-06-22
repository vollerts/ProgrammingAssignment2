## Matrix Inversion
# Objetive: cache the inverse of a matrix instead of repeated computation

# Test data and cmd sequence
# [1] x <- rbind(c(5,2),c(2,5))
# [2] str(x)
# [3] m <-makeCacheMatrix(x)
# [4] m$get()
# [5] cacheSolve(m) -- not cached yet
# [6] cacheSolve(m) -- cached

# Create a list of getters and setters for the matrix and the inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

# If inverse calculated before, return the inv object, else compute inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached")
    return(inv)
  }
  message("not cached yet")
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
